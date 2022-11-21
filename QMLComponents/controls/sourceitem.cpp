//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "sourceitem.h"

#include "analysisform.h"
#include "jasplistcontrol.h"
#include "models/listmodellabelvalueterms.h"
#include "log.h"
#include "rowcontrols.h"
#include <QQmlEngine>

SourceItem::SourceItem(
		  JASPListControl*							listControl
		, QMap<QString, QVariant>&					map
		, const JASPListControl::LabelValueMap&		values
		, const QVector<SourceItem*>				rSources
		, QAbstractItemModel*						nativeModel
		, const QVector<SourceItem*>&				discardSources
		, const QVector<QMap<QString, QVariant> >&	conditionVariables
		)
		: QObject(listControl), _listControl(listControl)
{
	QString modelUse			= map["use"].toString().trimmed();

	_name						= map["name"].toString();
	_controlName				= map["controlName"].toString();
	_modelUse					= !modelUse.isEmpty() ? modelUse.split(",") : QStringList();
	_conditionExpression		= map["condition"].toString();
	_values						= values;
	_nativeModel				= nativeModel;
	_discardSources				= discardSources;
	_rSources					= rSources;

	_isValuesSource				= map.contains("isValuesSource")			? map["isValuesSource"].toBool()			: false;
	_isVariableInfoModel		= map.contains("isDataSetVariables")		? map["isDataSetVariables"].toBool()		: false;
	_combineWithOtherModels		= map.contains("combineWithOtherModels")	? map["combineWithOtherModels"].toBool()	: false;
	_nativeModelRole			= map.contains("nativeModelRole")			? map["nativeModelRole"].toInt()			: Qt::DisplayRole;

	if (isInfoProviderModel(_nativeModel))									_isVariableInfoModel = true;
	if (_modelUse.contains("levels"))										_listControl->setUseSourceLevels(true);
	if (_listControl->useSourceLevels() && !_modelUse.contains("levels"))	_modelUse.append("levels");

	for (const QMap<QString, QVariant>& conditionVariable : conditionVariables)
	{
		_conditionVariables.push_back(ConditionVariable(conditionVariable["name"].toString()
									, conditionVariable["component"].toString()
									, conditionVariable["property"].toString()
									, conditionVariable["addQuotes"].toBool())
		);
	}

	_setUp();
}

SourceItem::SourceItem(JASPListControl *listControl, const JASPListControl::LabelValueMap &values)
	:  QObject(listControl), _listControl(listControl), _values(values), _isValuesSource(true)
{
	_setUp();
}

SourceItem::SourceItem(JASPListControl *listControl, const QString& rSourceName, const QString& modelUse)
	: QObject(listControl), _listControl(listControl), _name(rSourceName), _isRSource(true)
{
	if (!modelUse.isEmpty())	_modelUse = modelUse.split(".");

	_setUp();
}

SourceItem::SourceItem(JASPListControl *listControl)
	:  QObject(listControl), _listControl(listControl), _isVariableInfoModel(true)
{
	_setUp();
}

void SourceItem::connectModels()
{
	if (!_listControl->initialized() || _connected) return;

	ListModel *controlModel = _listControl->model();
	AnalysisForm* form		= _listControl->form();

	if (_isRSource && form)
		connect(form,	&AnalysisForm::rSourceChanged,				this, &SourceItem::_rSourceChanged);

	if (_nativeModel)
	{
		connect(_nativeModel, &QAbstractItemModel::dataChanged,			this, &SourceItem::_dataChangedHandler);
		connect(_nativeModel, &QAbstractItemModel::rowsInserted,		this, &SourceItem::_resetModel);
		connect(_nativeModel, &QAbstractItemModel::rowsRemoved,			this, &SourceItem::_resetModel);
		connect(_nativeModel, &QAbstractItemModel::rowsMoved,			this, &SourceItem::_resetModel);
		connect(_nativeModel, &QAbstractItemModel::modelReset,			this, &SourceItem::_resetModel);
	}

	if (_isVariableInfoModel)
	{
		VariableInfo* variableInfo = VariableInfo::info();
		connect(variableInfo,	&VariableInfo::namesChanged,		controlModel, &ListModel::sourceNamesChanged );
		connect(variableInfo,	&VariableInfo::columnTypeChanged,	controlModel, &ListModel::sourceColumnTypeChanged );
		connect(variableInfo,	&VariableInfo::labelsChanged,		controlModel, &ListModel::sourceLabelsChanged );
		connect(variableInfo,	&VariableInfo::labelsReordered,		controlModel, &ListModel::sourceLabelsReordered );
		connect(variableInfo,	&VariableInfo::columnsChanged,		controlModel, &ListModel::sourceColumnsChanged );
	}

	if (_listModel)
	{
		connect(_listModel,		&ListModel::namesChanged,			controlModel, &ListModel::sourceNamesChanged);
		connect(_listModel,		&ListModel::columnTypeChanged,		controlModel, &ListModel::sourceColumnTypeChanged);
		connect(_listModel,		&ListModel::labelsChanged,			controlModel, &ListModel::sourceLabelsChanged );
		connect(_listModel,		&ListModel::labelsReordered,		controlModel, &ListModel::sourceLabelsReordered );
		connect(_listModel,		&ListModel::columnsChanged,			controlModel, &ListModel::sourceColumnsChanged );
	}

	_connected = true;
}

void SourceItem::disconnectModels()
{
	if (!_connected) return;

	ListModel *controlModel = _listControl->model();
	AnalysisForm* form		= _listControl->form();

	if (_isRSource && form)
		disconnect(form,	&AnalysisForm::rSourceChanged,				this, &SourceItem::_rSourceChanged);

	if (_nativeModel)
		_nativeModel->disconnect(this);

	if (_isVariableInfoModel)
		infoProviderModel()->disconnect(controlModel);

	if (_listModel)
		_listModel->disconnect(controlModel);

	_connected = false;
}


void SourceItem::_resetModel()
{
	if (!_isVariableInfoModel || !requestInfo(VariableInfo::SignalsBlocked).toBool())
		_listControl->model()->sourceTermsReset();
}

void SourceItem::_dataChangedHandler(const QModelIndex &, const QModelIndex &, const QVector<int> &roles)
{
	// If the dataChanged is due to a selection, don't reset the model: it is just that the QML item should get the right color.
	if (roles.size() == 1 && roles.contains(ListModel::SelectedRole)) return;

	_resetModel();
}

void SourceItem::_rSourceChanged(const QString& name)
{
	if (_isRSource && name == _name)
		_listControl->model()->sourceTermsReset();
}

void SourceItem::_setUp()
{
	if (_isValuesSource)								_nativeModel = new ListModelLabelValueTerms(_listControl, _values);
	else if (_listControl->form() && !_name.isEmpty())	_nativeModel = _listControl->form()->getModel(_name);
	else if (_isVariableInfoModel)
	{
		_nativeModel		= infoProviderModel();
		_nativeModelRole	= requestInfo(VariableInfo::NameRole).toInt();
	}

	if (_nativeModel || _isRSource)
	{
		_listModel = qobject_cast<ListModel*>(_nativeModel);
		if (_listModel)	_listControl->addDependency(_listModel->listView());

		// Do not connect before this control (and the controls of the source) are completely initialized
		// The source could sent some data to this control before it is completely ready for it.
		if (_listControl->initialized()) connectModels();
		else connect(_listControl, &JASPControl::initializedChanged, this, &SourceItem::connectModels);
	}
	else if (_rSources.length() == 0)
	{
		if (_name.isEmpty())
		{
			if (_listControl->form())		_listControl->addControlError(QObject::tr("No name given for the source of %1").arg(_listControl->name()));
			else							_listControl->addControlError(QObject::tr("No source given for %1").arg(_listControl->name()));
		}
		else								_listControl->addControlError(QObject::tr("Cannot find component %1 for the source of %2").arg(_name).arg(_listControl->name()));
	}
}

QList<QVariant> SourceItem::getListVariant(QVariant var)
{
	QList<QVariant> listVar;

	if (!var.isValid() || var.isNull())
		return listVar;

	if(var.typeId() == QMetaType::QString)	listVar = QList<QVariant>({var});
	else									listVar = var.toList();

	if (listVar.isEmpty())
	{
		QStringList stringSources = var.toStringList();
		for (const QString& stringSource : stringSources)
			listVar.push_back(stringSource);
	}

	if (listVar.isEmpty())
	{
		if (var.canConvert<QMap<QString, QVariant> >())
		{
			QMap<QString, QVariant> map = var.toMap();
			listVar.push_back(map);
		}
	}

	if (listVar.isEmpty())
		listVar.push_back(var);

	return listVar;
}

QString SourceItem::_readSourceName(const QString& sourceNameExt, QString& sourceControl, QString& sourceUse)
{
	QStringList nameSplit = sourceNameExt.split(".");
	if (nameSplit.length() > 1)
	{
		sourceControl = nameSplit[1];
		sourceUse = "control=" + nameSplit[1];
	}

	return nameSplit[0];
}

QString SourceItem::_readRSourceName(const QString& sourceNameExt, QString& sourceUse)
{
	QStringList nameSplit = sourceNameExt.split(".");
	QString name = nameSplit[0];
	if (nameSplit.length() > 1)
	{
		nameSplit.removeAt(0);
		sourceUse = nameSplit.join(".");
	}

	return name;
}


QMap<QString, QVariant> SourceItem::_readSource(JASPListControl* listControl, const QVariant& source, JASPListControl::LabelValueMap& sourceValues, QVector<SourceItem*>& rSources, QAbstractItemModel*& nativeModel)
{
	QMap<QString, QVariant> map;
	QString sourceName, sourceControl, sourceUse;

	JASPControl* sourceItem = source.value<JASPControl*>();
	if (sourceItem)										sourceName = sourceItem->name();
	else if (source.type() == QVariant::Type::String)	sourceName = _readSourceName(source.toString(), sourceControl, sourceUse);
	else if (source.canConvert<QMap<QString, QVariant> >())
	{
		map = source.toMap();
		if (map.contains("id"))
		{
			JASPControl* sourceItem2 = map["id"].value<JASPControl*>();
			if (sourceItem2)	sourceName = sourceItem2->name();
		}

		if (map.contains("name"))
			sourceName = _readSourceName(map["name"].toString(), sourceControl, sourceUse);

		if (map.contains("use"))
		{
			if (!sourceUse.isEmpty())
				sourceUse += ",";
			sourceUse += map["use"].toString();
		}

		if (map.contains("values"))
		{
			sourceValues = _readValues(listControl, map["values"]);
			map["isValuesSource"] = true;
		}

		if (map.contains("rSource"))
		{
			QList<QVariant> rawRSources = getListVariant(map["rSource"]);
			for (const QVariant& rawSource : rawRSources)
				rSources.append(_readRSource(listControl, rawSource));
		}

		if (map.contains("model"))
			nativeModel = map["model"].value<QAbstractItemModel*>();
	}
	else
		nativeModel = source.value<QAbstractItemModel*>();


	if (nativeModel)
	{
		QString roleName = sourceUse.isEmpty() ? listControl->labelRole() : sourceUse;
		map["nativeModelRole"] = Qt::DisplayRole;

		if (!roleName.isEmpty())
		{
			QHashIterator<int, QByteArray> it(nativeModel->roleNames());

			while (it.hasNext())
			{
				it.next();
				if (it.value() == roleName)
				{
					map["nativeModelRole"] = it.key();
					break;
				}
			}
		}
	}
	map["name"]			= sourceName;
	map["controlName"]	= sourceControl;
	map["use"]			= sourceUse;
	return map;
}

JASPListControl::LabelValueMap SourceItem::_readValues(JASPListControl* listControl, const QVariant& values)
{
	JASPListControl::LabelValueMap result;

	bool isInteger = false;
	int count =  values.toInt(&isInteger);

	if (isInteger)
	{
		for (int i = 1; i <= count; i++)
		{
			QString number = QString::number(i);
			result.push_back(std::make_pair(number, number));
		}
	}
	else
	{
		QList<QVariant> list = values.toList();
		if (!list.isEmpty())
		{
			for (const QVariant& itemVariant : list)
			{
				QMap<QString, QVariant> labelValuePair = itemVariant.toMap();
				if (labelValuePair.isEmpty())
				{
					QString value = itemVariant.toString();
					result.push_back(std::make_pair(value, value));
				}
				else
				{
					QString label = labelValuePair[listControl->labelRole()].toString();
					QString value = labelValuePair[listControl->valueRole()].toString();
					result.push_back(std::make_pair(label, value));
				}
			}
		}
	}

	return result;
}

SourceItem* SourceItem::_readRSource(JASPListControl* listControl, const QVariant& rSource)
{
	QString sourceName, sourceUse;
	QMap<QString, QVariant> map;

	if (rSource.type() == QVariant::Type::String)	sourceName = _readRSourceName(rSource.toString(), sourceUse);
	else if (rSource.canConvert<QMap<QString, QVariant> >())
	{
		map = rSource.toMap();
		if (map.contains("name"))
			sourceName = _readRSourceName(map["name"].toString(), sourceUse);

		if (map.contains("use"))
		{
			if (!sourceUse.isEmpty())
				sourceUse += ".";
			sourceUse += map["use"].toString().trimmed();
		}
	}

	return new SourceItem(listControl, sourceName, sourceUse);
}

QVector<SourceItem*> SourceItem::readAllSources(JASPListControl* listControl)
{
	QVector<SourceItem*> sources;

	if (listControl->values().isValid() && !listControl->values().isNull())
		sources.append(new SourceItem(listControl, _readValues(listControl, listControl->values())));

	if (listControl->rSource().isValid() && !listControl->rSource().isNull())
	{
		QList<QVariant> rSources = getListVariant(listControl->rSource());
		for (const QVariant& rSource : rSources)
			sources.append(_readRSource(listControl, rSource));
	}

	QList<QVariant> rawSources = getListVariant(listControl->source());

	for (const QVariant& rawSource : rawSources)
	{
		JASPListControl::LabelValueMap sourceValues;
		QVector<SourceItem*> rSources;
		QAbstractItemModel* nativeModel = nullptr;
		QMap<QString, QVariant> map = _readSource(listControl, rawSource, sourceValues, rSources, nativeModel);
		QVector<SourceItem*> discards;
		QVector<QMap<QString, QVariant> > conditionVariables;

		if (map.contains("discard"))
		{
			QList<QVariant> discardSources = getListVariant(map["discard"]);

			for (const QVariant& discardSource : discardSources)
			{
				JASPListControl::LabelValueMap discardValues;
				QVector<SourceItem*> discardRSources;
				QAbstractItemModel* discardNativeModel = nullptr;
				QMap<QString, QVariant> discardMap = _readSource(listControl, discardSource, discardValues, discardRSources, discardNativeModel);

				discards.push_back(new SourceItem(listControl, discardMap, discardValues, discardRSources, discardNativeModel));
			}
		}

		if (map.contains("conditionVariables"))
		{
			QList<QVariant> conditionVariablesList = getListVariant(map["conditionVariables"]);
			for (const QVariant& conditionVariablesVar : conditionVariablesList)
				if (conditionVariablesVar.canConvert<QMap<QString, QVariant> >())
					conditionVariables.push_back(conditionVariablesVar.toMap());
		}

		sources.append(new SourceItem(listControl, map, sourceValues, rSources, nativeModel, discards, conditionVariables));
	}

	if (sources.isEmpty() && listControl->model()->needsSource())
		sources.append(new SourceItem(listControl)); // Add all columns source

	return sources;
}

Terms SourceItem::_readAllTerms()
{
	Terms terms;

	if (_isRSource)
		terms = _listControl->form()->getValuesFromRSource(_name, _modelUse);
	else if (_rSources.length() > 0)
	{
		for (SourceItem* rSource : _rSources)
			terms.add(rSource->getTerms());
	}
	else if (_listModel)
	{
		terms = _listModel->termsEx(_modelUse);
		if (_listControl->useSourceLevels())
			_listControl->model()->setColumnsUsedForLabels(_listModel->terms().asQList());
	}
	else if (_isVariableInfoModel)
		terms = requestInfo(VariableInfo::VariableNames).toStringList();
	else if (_nativeModel)
	{
		int nbRows = _nativeModel->rowCount();
		int nbCols = _nativeModel->columnCount();
		for (int i = 0; i < nbRows; i++)
		{
			QStringList row;
			for (int j = 0; j < nbCols; j++)
				row.append(_nativeModel->data(_nativeModel->index(i, j), _nativeModelRole).toString());
			terms.add(Term(row), false);
		}
		if (!_modelUse.empty())
			// If the 'use' parameter of the source property asks for the levels, or to filter some types
			// of the variables of this 'native' model (probably the columnsModel),
			// then just use the filterTerms method of the model object of the current control.
			terms = _listControl->model()->filterTerms(terms, _modelUse);
	}

	if (_combineTerms != JASPControl::CombinationType::NoCombination)
		terms = terms.combineTerms(_combineTerms);

	if (_onlyTermsWithXComponents > 0)
	{
		Terms termsWithOnlyXComponents;
		for (const Term & term : terms)
			if (term.size() == _onlyTermsWithXComponents)
				termsWithOnlyXComponents.add(term);

		terms = termsWithOnlyXComponents;
	}

	return terms;
}

Terms SourceItem::filterTermsWithCondition(ListModel* model, const Terms& terms, const QString& condition, const QVector<ConditionVariable>& conditionVariables)
{
	Terms filteredTerms;
	JASPListControl* listControl = model->listView();
	QJSEngine* jsEngine = qmlEngine(listControl);

	for (const Term& term : terms)
	{
		if (conditionVariables.length() > 0)
		{
			// If condition variables are used, use them
			for (const ConditionVariable& conditionVariable : conditionVariables)
			{
				JASPControl* control = model->getRowControl(term.asQString(), conditionVariable.controlName);
				if (control)
				{
					QJSValue value;
					QVariant valueVar = control->property(conditionVariable.propertyName.toStdString().c_str());

					switch (valueVar.type())
					{
					case QVariant::Type::Int:
					case QVariant::Type::UInt:		value = valueVar.toInt();		break;
					case QVariant::Type::Double:	value = valueVar.toDouble();	break;
					case QVariant::Type::Bool:		value = valueVar.toBool();		break;
					default:						value = valueVar.toString();	break;
					}

					jsEngine->globalObject().setProperty(conditionVariable.name, value);
				}
			}
		}
		else
		{
			// If no condition variables are used, then set the values of the row controls in variables
			RowControls* rowControls = model->getRowControls(term.asQString());
			if (!rowControls) 
				continue;

			for (const QString& variable : rowControls->getJASPControlsMap().keys())
			{
				JASPControl * control 		= rowControls->getJASPControl(variable);
				BoundControl* boundControl 	= control ? control->boundControl() : nullptr;

				if (boundControl)
				{
					QJSValue value;
					bool 				addValue  = true;
					const Json::Value & jsonValue = boundControl->boundValue();

					switch (jsonValue.type())
					{
					case Json::booleanValue:		value = jsonValue.asBool();			break;
					case Json::uintValue:			value = jsonValue.asUInt();			break;
					case Json::intValue:			value = jsonValue.asInt();			break;
					case Json::realValue:			value = jsonValue.asDouble();		break;
					case Json::stringValue:			value = tq(jsonValue.asString());	break;
					default:						addValue = false;					break;
					}

					if (addValue)
						jsEngine->globalObject().setProperty(variable, value);
				}
			}
		}

		QJSValue result = jsEngine->evaluate(condition);

		if (result.isError())
			listControl->addControlError("Error when evaluating : " + condition + ": " + result.toString());

		else if (result.toBool())
			filteredTerms.add(term);
	}

	return filteredTerms;
}

Terms SourceItem::getTerms()
{
	Terms sourceTerms = _readAllTerms();

	for (SourceItem* discardModel : _discardSources)
		sourceTerms.discardWhatDoesContainTheseComponents(discardModel->_readAllTerms());

	if (!_conditionExpression.isEmpty() && _listModel)
		sourceTerms = filterTermsWithCondition(_listModel, sourceTerms, _conditionExpression, _conditionVariables);

	return sourceTerms;
}

QSet<QString> SourceItem::usedControls() const
{
	QSet<QString> result;

	if (!_controlName.isEmpty()) 
		result.insert(_controlName);
		
	if (_conditionVariables.length() > 0)
	{
		for (const ConditionVariable& conditionVariable : _conditionVariables)
			if (!conditionVariable.controlName.isEmpty())
				result.insert(conditionVariable.controlName);
	}
	else if (!_conditionExpression.isEmpty() && _listModel->getAllRowControls().size() > 0)
	{
		// Take the controls of the first row, and check whether the expression contains their names.
		RowControls * rowControls = _listModel->getAllRowControls().begin().value();

		for (const QString & controlName : rowControls->getJASPControlsMap().keys())
			if (_conditionExpression.contains(controlName)) 
				result.insert(controlName);
	}

	return result;
}
