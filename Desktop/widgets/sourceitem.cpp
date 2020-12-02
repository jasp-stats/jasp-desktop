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

#include "analysis/analysisform.h"
#include "jasplistcontrol.h"
#include "listmodellabelvalueterms.h"
#include <QQmlEngine>

SourceItem::SourceItem(
		  JASPListControl*							listControl
		, QMap<QString, QVariant>&					map
		, const JASPListControl::LabelValueMap&		values
		, QAbstractItemModel*						nativeModel
		, const QVector<SourceItem*>&				discardSources
		, const QVector<QMap<QString, QVariant> >&	conditionVariables
		) : _listControl(listControl)
{
	_name					= map["name"].toString();
	_controlName			= map["controlName"].toString();
	_modelUse				= map["use"].toString();
	_conditionExpression	= map["condition"].toString();
	_values					= values;
	_nativeModel			= nativeModel;
	_discardSources			= discardSources;

	_isValuesSource			= map.contains("isValuesSource")			? map["isValuesSource"].toBool()			: false;
	_isDataSetColumns		= map.contains("isDataSetColumns")			? map["isDataSetColumns"].toBool()			: false;
	_combineWithOtherModels	= map.contains("combineWithOtherModels")	? map["combineWithOtherModels"].toBool()	: false;
	_nativeModelRole		= map.contains("nativeModelRole")			? map["nativeModelRole"].toInt()			: Qt::DisplayRole;

	if (!_controlName.isEmpty()) _usedControls.insert(_controlName);


	for (const QMap<QString, QVariant>& conditionVariable : conditionVariables)
	{
		_conditionVariables.push_back(ConditionVariable(conditionVariable["name"].toString()
									, conditionVariable["component"].toString()
									, conditionVariable["property"].toString()
									, conditionVariable["addQuotes"].toBool())
					);
		if (!conditionVariable["component"].toString().isEmpty()) _usedControls.insert(conditionVariable["component"].toString());
	}

	_setUp();
}

SourceItem::SourceItem(JASPListControl *listControl, const JASPListControl::LabelValueMap &values)
	:  _listControl(listControl), _values(values), _isValuesSource(true)
{
	_setUp();
}

SourceItem::SourceItem(JASPListControl *listControl)
	:  _listControl(listControl), _isDataSetColumns(true)
{
	_setUp();
}


void SourceItem::_setUp()
{
	ListModel* sourceModel = nullptr;

	if (_isDataSetColumns)
	{
		_nativeModel = AnalysisForm::getColumnsModel();
		_nativeModelRole = AnalysisForm::getColumnsModelRole();
	}

	if (_isValuesSource)								sourceModel = new ListModelLabelValueTerms(_listControl, _values);
	else if (_nativeModel)								sourceModel = qobject_cast<ListModel*>(_nativeModel); // A nativeModel is not always a ListModel
	else if (_listControl->form() && !_name.isEmpty())	sourceModel = _listControl->form()->getModel(_name);

	if (sourceModel)
	{
		_model = sourceModel;
		_listControl->addDependency(_model->listView());
		ListModel::connect(_model, &ListModel::termsChanged, _listControl->model(), &ListModel::sourceTermsChanged);
	}
	else if (_nativeModel)
	{
		connect(_nativeModel, &QAbstractItemModel::dataChanged,			this,	[this]() { _listControl->model()->sourceTermsChanged(); });
		connect(_nativeModel, &QAbstractItemModel::rowsInserted,		this,	[this]() { _listControl->model()->sourceTermsChanged(); });
		connect(_nativeModel, &QAbstractItemModel::rowsRemoved,			this,	[this]() { _listControl->model()->sourceTermsChanged(); });
		connect(_nativeModel, &QAbstractItemModel::modelReset,			this,	[this]() { _listControl->model()->sourceTermsChanged(); });
	}
	else
	{
		if (_name.isEmpty())
		{
			if (_listControl->form())		_listControl->addControlError(QObject::tr("No name given for the source of %1").arg(_listControl->name()));
			else							_listControl->addControlError(QObject::tr("No source given for %1").arg(_listControl->name()));
		}
		else								_listControl->addControlError(QObject::tr("Cannot find component %1 for the source of %2").arg(_name).arg(_listControl->name()));
	}
}

SourceItem::~SourceItem()
{
	isAlive = false;
	if (_isValuesSource)
		delete _model; // In case of values, the model is created just to contain the values, and does not come from another listview.
	else if (_model)
	{
		_listControl->removeDependency(_model->listView());
		ListModel::disconnect(_model, &ListModel::termsChanged, _listControl->model(), &ListModel::sourceTermsChanged);
	}

	for (SourceItem* discardModel : _discardSources)
		delete discardModel;
}

QList<QVariant> SourceItem::_getListVariant(QVariant var)
{
	QList<QVariant> listVar;

	if (!var.isValid() || var.isNull())
		return listVar;

	listVar = var.toList();

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

QMap<QString, QVariant> SourceItem::_readSource(JASPListControl* listControl, const QVariant& source, JASPListControl::LabelValueMap& sourceValues, QAbstractItemModel*& nativeModel)
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

		if (map.contains("model"))
			nativeModel = source.value<QAbstractItemModel*>();
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

QVector<SourceItem*> SourceItem::readAllSources(JASPListControl* listControl)
{
	QVector<SourceItem*> sources;

	if (listControl->values().isValid() && !listControl->values().isNull())
		sources.append(new SourceItem(listControl, _readValues(listControl, listControl->values())));

	QList<QVariant> rawSources = _getListVariant(listControl->source());

	for (const QVariant& rawSource : rawSources)
	{
		JASPListControl::LabelValueMap sourceValues;
		QAbstractItemModel* nativeModel = nullptr;
		QMap<QString, QVariant> map = _readSource(listControl, rawSource, sourceValues, nativeModel);
		QVector<SourceItem*> discards;
		QVector<QMap<QString, QVariant> > conditionVariables;

		if (map.contains("discard"))
		{
			QList<QVariant> discardSources = _getListVariant(map["discard"]);

			for (const QVariant& discardSource : discardSources)
			{
				JASPListControl::LabelValueMap discardValues;
				QAbstractItemModel* discardNativeModel = nullptr;
				QMap<QString, QVariant> discardMap = _readSource(listControl, discardSource, discardValues, discardNativeModel);

				discards.push_back(new SourceItem(listControl, discardMap, discardValues, discardNativeModel));
			}
		}

		if (map.contains("conditionVariables"))
		{
			QList<QVariant> conditionVariablesList = _getListVariant(map["conditionVariables"]);

			for (const QVariant& conditionVariablesVar : conditionVariablesList)
				if (conditionVariablesVar.canConvert<QMap<QString, QVariant> >())
					conditionVariables.push_back(conditionVariablesVar.toMap());
		}

		sources.append(new SourceItem(listControl, map, sourceValues, nativeModel, discards, conditionVariables));
	}

	if (sources.isEmpty() && listControl->model()->needsSource())
		sources.append(new SourceItem(listControl)); // Add all columns source

	return sources;
}

Terms SourceItem::_readAllTerms()
{
	Terms terms;

	if (_model)	terms = _model->terms(_modelUse);
	else if (_nativeModel)
	{
		int nbRows = _nativeModel->rowCount();
		for (int i = 0; i < nbRows; i++)
		{
			QModelIndex modelIndex(_nativeModel->index(i, 0));
			terms.add(_nativeModel->data(modelIndex, _nativeModelRole).toString());
		}
	}

	return terms;
}

Terms SourceItem::getTerms()
{
	Terms sourceTerms = _readAllTerms();

	for (SourceItem* discardModel : _discardSources)
		sourceTerms.discardWhatDoesContainTheseComponents(discardModel->_readAllTerms());

	if (!_conditionExpression.isEmpty() && _model)
	{
		Terms filteredTerms;
		QJSEngine* jsEngine = qmlEngine(_listControl);

		for (const Term& term : sourceTerms)
		{
			for (const ConditionVariable& conditionVariable : _conditionVariables)
			{
				JASPControl* control = _model->getRowControl(term.asQString(), conditionVariable.controlName);
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

			QJSValue result = jsEngine->evaluate(_conditionExpression);
			if (result.isError())
				_listControl->addControlError("Error when evaluating : " + _conditionExpression + ": " + result.toString());
			else if (result.toBool())
				filteredTerms.add(term);
		}

		sourceTerms = filteredTerms;
	}

	return sourceTerms;
}
