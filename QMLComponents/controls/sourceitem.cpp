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

const QString SourceItem::SourceValueLabel = "label";
const QString SourceItem::SourceValueValue = "value";
const QString SourceItem::SourceValueInfo = "info";

SourceItem::SourceItem(
		  JASPListControl*							targetListControl
		, QMap<QString, QVariant>&					map
		, const SourceValuesType&					values
		, const QVector<SourceItem*>				rSources
		, QAbstractItemModel*						nativeModel
		, const QVector<SourceItem*>&				discardSources
		, const QVector<QMap<QString, QVariant> >&	conditionVariables
		)
		: QObject(targetListControl), _targetListControl(targetListControl)
{
	QString modelUse			= map["use"].toString().trimmed();

	_sourceName					= map["name"].toString();
	_rowControlName				= map["controlName"].toString();
	_sourceFilter				= !modelUse.isEmpty() ? modelUse.split(",") : QStringList();
	_conditionExpression		= map["condition"].toString();
	_values						= values;
	_sourceNativeModel			= nativeModel;
	_discardSources				= discardSources;
	_rSources					= rSources;

	_isValuesSource				= map.contains("isValuesSource")			? map["isValuesSource"].toBool()			: false;
	_isDataSetVariables			= map.contains("isDataSetVariables")		? map["isDataSetVariables"].toBool()		: false;
	_combineWithOtherModels		= map.contains("combineWithOtherModels")	? map["combineWithOtherModels"].toBool()	: false;
	_noInteractions				= map.contains("noInteraction")				? map["noInteraction"].toBool()				: false;
	_nativeModelRole			= map.contains("nativeModelRole")			? map["nativeModelRole"].toInt()			: Qt::DisplayRole;
	_combineTerms				= map.contains("combineTerms")				? JASPControl::CombinationType(map["combineTerms"].toInt())	: JASPControl::CombinationType::NoCombination;
	if (isInfoProviderModel(_sourceNativeModel))									_isDataSetVariables = true;
	if (_sourceFilter.contains("levels"))											_targetListControl->setUseSourceLevels(true);
	if (_targetListControl->useSourceLevels() && !_sourceFilter.contains("levels"))	_sourceFilter.append("levels");

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

SourceItem::SourceItem(JASPListControl *listControl, const SourceItem::SourceValuesType &values)
	:  QObject(listControl), _targetListControl(listControl), _values(values), _isValuesSource(true)
{
	_setUp();
}

SourceItem::SourceItem(JASPListControl *listControl, const QString& rSourceName, const QString& modelUse)
	: QObject(listControl), _targetListControl(listControl), _sourceName(rSourceName), _isRSource(true)
{
	if (!modelUse.isEmpty())	_sourceFilter = modelUse.split(".");

	_setUp();
}

SourceItem::SourceItem(JASPListControl *listControl)
	:  QObject(listControl), _targetListControl(listControl), _isDataSetVariables(true)
{
	_setUp();
}

void SourceItem::connectModels()
{
	if (!_targetListControl->initialized() || _connected) return;

	ListModel *controlModel = _targetListControl->model();
	AnalysisForm* form		= _targetListControl->form();

	if (_isRSource && form)
		connect(form,	&AnalysisForm::rSourceChanged,						this, &SourceItem::_rSourceChanged);

	if (_sourceNativeModel)
	{
		connect(_sourceNativeModel, &QAbstractItemModel::dataChanged,		this, &SourceItem::_dataChangedHandler);
		connect(_sourceNativeModel, &QAbstractItemModel::rowsInserted,		this, &SourceItem::_resetModel);
		connect(_sourceNativeModel, &QAbstractItemModel::rowsRemoved,		this, &SourceItem::_resetModel);
		connect(_sourceNativeModel, &QAbstractItemModel::rowsMoved,			this, &SourceItem::_resetModel);
		connect(_sourceNativeModel, &QAbstractItemModel::modelReset,		this, &SourceItem::_resetModel);
	}
	if (_targetListControl->useSourceLevels() && _sourceNativeModel != infoProviderModel())
	{
		QAbstractItemModel* providerModel = infoProviderModel(); // When the levels/labels of the source is used, then any change of the provider model must also be signalled
		connect(providerModel, &QAbstractItemModel::rowsInserted,			this, &SourceItem::_resetModel);
		connect(providerModel, &QAbstractItemModel::rowsRemoved,			this, &SourceItem::_resetModel);
		connect(providerModel, &QAbstractItemModel::rowsMoved,				this, &SourceItem::_resetModel);
		connect(providerModel, &QAbstractItemModel::modelReset,				this, &SourceItem::_resetModel);
	}

	if (_isDataSetVariables)
	{
		VariableInfo* variableInfo = VariableInfo::info();
		connect(variableInfo,	&VariableInfo::namesChanged,		controlModel, &ListModel::sourceNamesChanged );
		connect(variableInfo,	&VariableInfo::columnTypeChanged,	controlModel, [this, controlModel] (QString colName)
		{
			Term term(colName, (columnType)requestInfo(VariableInfo::VariableType, colName).toInt());
			controlModel->sourceColumnTypeChanged(term);
		} );
		connect(variableInfo,	&VariableInfo::labelsChanged,		controlModel, &ListModel::sourceLabelsChanged );
		connect(variableInfo,	&VariableInfo::labelsReordered,		controlModel, &ListModel::sourceLabelsReordered );
		connect(variableInfo,	&VariableInfo::filterChanged,		controlModel, &ListModel::filterChanged );
		connect(variableInfo,	&VariableInfo::columnsChanged,		controlModel, &ListModel::sourceColumnsChanged );
	}

	if (_sourceListModel)
	{
		connect(_sourceListModel,		&ListModel::namesChanged,			controlModel, &ListModel::sourceNamesChanged);
		connect(_sourceListModel,		&ListModel::columnTypeChanged,		controlModel, &ListModel::sourceColumnTypeChanged);
		connect(_sourceListModel,		&ListModel::labelsChanged,			controlModel, &ListModel::sourceLabelsChanged );
		connect(_sourceListModel,		&ListModel::labelsReordered,		controlModel, &ListModel::sourceLabelsReordered );
		connect(_sourceListModel,		&ListModel::filterChanged,			controlModel, &ListModel::filterChanged );
		connect(_sourceListModel,		&ListModel::columnsChanged,			controlModel, &ListModel::sourceColumnsChanged );
	}

	_connected = true;
}

void SourceItem::disconnectModels()
{
	if (!_connected) return;

	ListModel *controlModel = _targetListControl->model();
	AnalysisForm* form		= _targetListControl->form();

	if (_isRSource && form)
		disconnect(form,	&AnalysisForm::rSourceChanged,				this, &SourceItem::_rSourceChanged);

	if (_sourceNativeModel)
		_sourceNativeModel->disconnect(this);

	if (_isDataSetVariables)
		infoProviderModel()->disconnect(controlModel);

	if (_sourceListModel)
		_sourceListModel->disconnect(controlModel);

	_connected = false;
}


void SourceItem::_resetModel()
{
	if (!_isDataSetVariables || !requestInfo(VariableInfo::SignalsBlocked).toBool())
		_targetListControl->model()->sourceTermsReset();
}

void SourceItem::_dataChangedHandler(const QModelIndex &, const QModelIndex &, const QVector<int> &roles)
{
	// If the dataChanged is due to a selection, don't reset the model: it is just that the QML item should get the right color.
	if (roles.size() == 1 && roles.contains(ListModel::SelectedRole)) return;

	_resetModel();
}

void SourceItem::_rSourceChanged(const QString& name)
{
	if (_isRSource && name == _sourceName)
		_targetListControl->model()->sourceTermsReset();
}

void SourceItem::_setUp()
{
	if (_isValuesSource)											_sourceNativeModel = new ListModelLabelValueTerms(_targetListControl, _values);
	else if (_targetListControl->form() && !_sourceName.isEmpty())	_sourceNativeModel = _targetListControl->form()->getModel(_sourceName);
	else if (_isDataSetVariables)
	{
		_sourceNativeModel		= infoProviderModel();
		_nativeModelRole	= requestInfo(VariableInfo::NameRole).toInt();
	}

	if (_sourceNativeModel || _isRSource)
	{
		_sourceListModel = qobject_cast<ListModel*>(_sourceNativeModel);
		if (_sourceListModel)	_targetListControl->addDependency(_sourceListModel->listView());

		// Do not connect before this control (and the controls of the source) are completely initialized
		// The source could sent some data to this control before it is completely ready for it.
		if (_targetListControl->initialized()) connectModels();
		else connect(_targetListControl, &JASPControl::initializedChanged, this, &SourceItem::connectModels);
	}
	else if (_rSources.length() == 0)
	{
		if (_sourceName.isEmpty())
		{
			if (_targetListControl->form())		_targetListControl->addControlError(QObject::tr("No name given for the source of %1").arg(_targetListControl->name()));
			else							_targetListControl->addControlError(QObject::tr("No source given for %1").arg(_targetListControl->name()));
		}
		else								_targetListControl->addControlError(QObject::tr("Cannot find component %1 for the source of %2").arg(_sourceName).arg(_targetListControl->name()));
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


QMap<QString, QVariant> SourceItem::_readSource(JASPListControl* listControl, const QVariant& source, SourceItem::SourceValuesType& sourceValues, QVector<SourceItem*>& rSources, QAbstractItemModel*& nativeModel)
{
	QMap<QString, QVariant> map;
	QString sourceName, sourceControl, sourceUse;

	JASPControl* sourceItem = source.value<JASPControl*>();
	if (sourceItem)										sourceName = sourceItem->name();
	else if (source.typeId() == QMetaType::QString)	sourceName = _readSourceName(source.toString(), sourceControl, sourceUse);
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
			QString useAttribute = map["use"].toString();
			if (useAttribute == "noInteraction")
				map["noInteraction"] = true;
			else
			{
				if (!sourceUse.isEmpty())
					sourceUse += ",";
				sourceUse += map["use"].toString();
			}
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
		QString roleName = sourceUse.isEmpty() ? SourceItem::SourceValueLabel : sourceUse;
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

SourceItem::SourceValuesType SourceItem::_readValues(JASPListControl* listControl, const QVariant& values)
{
	SourceItem::SourceValuesType result;

	bool isInteger = false;
	int count =  values.toInt(&isInteger);

	if (isInteger)
	{
		for (int i = 1; i <= count; i++)
		{
			QString number = QString::number(i);
			result.push_back(SourceValuesItem(number, number, ""));
		}
	}
	else
	{
		QList<QVariant> list = values.toList();
		if (!list.isEmpty())
		{
			for (const QVariant& itemVariant : list)
			{
				QMap<QString, QVariant> labelValueMap = itemVariant.toMap();
				if (labelValueMap.isEmpty())
				{
					QString value = itemVariant.toString();
					result.push_back(SourceValuesItem(value, value, ""));
				}
				else
				{
					QString label = labelValueMap[SourceItem::SourceValueLabel].toString();
					QString value = labelValueMap[SourceItem::SourceValueValue].toString();
					QString info = labelValueMap[SourceItem::SourceValueInfo].toString();
					result.push_back(SourceValuesItem(label, value, info));
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

	if (rSource.typeId() == QMetaType::QString)	sourceName = _readRSourceName(rSource.toString(), sourceUse);
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
		SourceItem::SourceValuesType sourceValues;
		QVector<SourceItem*> rSources;
		QAbstractItemModel* nativeModel = nullptr;
		QMap<QString, QVariant> map = _readSource(listControl, rawSource, sourceValues, rSources, nativeModel);
		QVector<SourceItem*> discards;
		QVector<QMap<QString, QVariant> > conditionVariables;

		if (map.contains("discard") || map.contains("discardSource"))
		{
			QList<QVariant> discardSources = map.contains("discard") ? getListVariant(map["discard"]) : getListVariant(map["discardSource"]);

			for (const QVariant& discardSource : discardSources)
			{
				SourceItem::SourceValuesType discardValues;
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
		terms = _targetListControl->form()->getValuesFromRSource(_sourceName, _sourceFilter);
	else if (_rSources.length() > 0)
	{
		for (SourceItem* rSource : _rSources)
			terms.add(rSource->getTerms());
	}
	else if (_sourceListModel)
	{
		terms = _sourceListModel->termsEx(_sourceFilter);
		if (_targetListControl->useSourceLevels())
			_targetListControl->model()->setColumnsUsedForLabels(_sourceListModel->terms().asQList());
		if (_noInteractions)
		{
			Terms termsWithoutInteraction;
			for (const Term& term : terms)
				if (term.components().size() == 1)
					termsWithoutInteraction.add(term);
			terms = termsWithoutInteraction;
		}
	}
	else if (_isDataSetVariables)
	{
		QStringList variableNames = requestInfo(VariableInfo::VariableNames).toStringList();
		for (const QString& name : variableNames)
		{
			Term term(name, columnType(requestInfo(VariableInfo::VariableType, name).toInt()));
			terms.add(term);
		}
		if (!_sourceFilter.empty())
			// If the 'use' parameter of the source property asks for the levels, or to filter some types
			// of the variables of this 'native' model (probably the columnsModel),
			// then just use the filterTerms method of the model object of the current control.
			terms = _targetListControl->model()->filterTerms(terms, _sourceFilter);

	}
	else if (_sourceNativeModel)
	{
		int nbRows = _sourceNativeModel->rowCount();
		int nbCols = _sourceNativeModel->columnCount();
		for (int i = 0; i < nbRows; i++)
		{
			QStringList row;
			columnTypeVec types;
			for (int j = 0; j < nbCols; j++)
			{
				QString name = _sourceNativeModel->data(_sourceNativeModel->index(i, j), _nativeModelRole).toString();
				row.append(name);
				types.push_back(columnType(requestInfo(VariableInfo::VariableType, name).toInt()));
			}
			Term term(row, types);
			terms.add(term, false);
		}
		if (!_sourceFilter.empty())
			terms = _targetListControl->model()->filterTerms(terms, _sourceFilter);
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

Terms SourceItem::filterTermsWithCondition(ListModel* model, const Terms& terms, const QString& condition, const QVector<ConditionVariable>& conditionVariables, const QMap<QString, QStringList>& termsMap)
{
	Terms filteredTerms;
	JASPListControl* listControl = model->listView();
	QJSEngine* jsEngine = qmlEngine(listControl);

	for (const Term& term : terms)
	{
		QString		value = term.asQString();
		// There might be several original values: see jaspTestModule, "Test Sources with special attributes" analysis, "Source with controls" section
		QStringList	originalValues = termsMap.contains(value) ? termsMap[value] : QStringList{value};

		if (conditionVariables.length() > 0)
		{
			// If condition variables are used, use them
			for (const ConditionVariable& conditionVariable : conditionVariables)
			{
				QJSValue value;
				for (const QString& originalValue : originalValues)
				{
					JASPControl* control = model->getRowControl(originalValue, conditionVariable.controlName);
					if (control)
					{
						QVariant valueVar = control->property(conditionVariable.propertyName.toStdString().c_str());

						switch (valueVar.typeId())
						{
						case QMetaType::Int:
						case QMetaType::UInt:		value = valueVar.toInt();		break;
						case QMetaType::Double:		value = valueVar.toDouble();	break;
							// If at least one of the control is true, then the condition should be true. We can think of adding an extra attribute in the source property to be able to change this rule,
							// but yeah, I cannot come up already with an understandable attribute name for the user, so it means that this situation is really really peculiar.
							// Again, to understand the situation, look at the jaspTestModule, "Test Sources with special attributes" analysis, "Source with controls" section
						case QMetaType::Bool:		value = (value.isBool() ? value.toBool() : false) || valueVar.toBool();		break;
						default:					value = valueVar.toString();	break;
						}

					}
				}
				jsEngine->globalObject().setProperty(conditionVariable.name, value);
			}
		}
		else
		{
			// If no condition variables are used, then set the values of the row controls in variables
			QMap<QString, QJSValue> conditionValues;

			for (const QString& originalValue : originalValues)
			{
				RowControls* rowControls = model->getRowControls(originalValue);
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
						{
							// Same remark as above, when condition variables are used
							if (value.isBool() && conditionValues.contains(variable) && conditionValues[variable].isBool())
								value = value.toBool() || conditionValues[variable].toBool();
							conditionValues[variable] = value;
						}
						else
							Log::log() << "When evaluating condition '" << condition << "' the control " << control->name() << " could not be used because it has not a usable option: " << jsonValue.toStyledString() << std::endl;
					}
				}

				for (const QString& variable : conditionValues.keys())
					jsEngine->globalObject().setProperty(variable, conditionValues[variable]);
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

	if (!_conditionExpression.isEmpty() && _sourceListModel)
	{
		QMap<QString, QStringList> map;
		if (!_rowControlName.isEmpty()) //The sourceTerms are the values of this control, but to filter we need the values of the source
		{
			QStringList controlValues = sourceTerms.asQList();
			for (const QString& sourceValue : _sourceListModel->terms().asQList())
			{
				JASPControl* control = _sourceListModel->getRowControl(sourceValue, _rowControlName);
				if (control)
				{
					QString controlValue = control->property("value").toString();
					if (controlValues.contains(controlValue))
					{
						QStringList list = map[controlValue];
						list.push_back(sourceValue);
						map[controlValue] = list;
					}
				}
			}
		}

		sourceTerms = filterTermsWithCondition(_sourceListModel, sourceTerms, _conditionExpression, _conditionVariables, map);
	}

	return sourceTerms;
}

QSet<QString> SourceItem::usedControls() const
{
	QSet<QString> result;

	if (!_rowControlName.isEmpty())
		result.insert(_rowControlName);
		
	if (_conditionVariables.length() > 0)
	{
		for (const ConditionVariable& conditionVariable : _conditionVariables)
			if (!conditionVariable.controlName.isEmpty())
				result.insert(conditionVariable.controlName);
	}
	else if (!_conditionExpression.isEmpty() && _sourceListModel->getAllRowControls().size() > 0)
	{
		// Take the controls of the first row, and check whether the expression contains their names.
		RowControls * rowControls = _sourceListModel->getAllRowControls().begin().value();

		for (const QString & controlName : rowControls->getJASPControlsMap().keys())
			if (_conditionExpression.contains(controlName)) 
				result.insert(controlName);
	}

	return result;
}
