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

#include "jasplistcontrol.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrol.h"
#include "listmodel.h"
#include "listmodellabelvalueterms.h"
#include "log.h"
#include "rowcontrols.h"

#include <QQmlContext>

const QString JASPListControl::_defaultKey = "_JASPDefaultKey";


JASPListControl::JASPListControl(QQuickItem *parent)
	: JASPControl(parent)
{
}

void JASPListControl::setUpModel()
{
	emit modelChanged();
}

QList<QVariant> JASPListControl::_getListVariant(QVariant var)
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

QString JASPListControl::_readSourceName(const QString& sourceNameExt, QString& sourceControl, QString& sourceUse)
{
	QStringList nameSplit = sourceNameExt.split(".");
	if (nameSplit.length() > 1)
	{
		sourceControl = nameSplit[1];
		sourceUse = "control=" + nameSplit[1];
	}

	return nameSplit[0];
}

QMap<QString, QVariant> JASPListControl::_readSource(const QVariant& source, QString& sourceName, QString& sourceControl, QString& sourceUse, LabelValueMap& sourceValues)
{
	QMap<QString, QVariant> map;

	JASPControl* sourceItem = source.value<JASPControl*>();
	if (sourceItem)
		sourceName = sourceItem->name();
	else if (source.type() == QVariant::Type::String)
		sourceName = _readSourceName(source.toString(), sourceControl, sourceUse);
	else if (source.canConvert<QMap<QString, QVariant> >())
	{
		map = source.toMap();
		if (map.contains("id"))
		{
			JASPControl* sourceItem2 = map["id"].value<JASPControl*>();
			if (sourceItem2)
				sourceName = sourceItem2->name();
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
			sourceValues = _readValues(map["values"]);
	}

	return map;
}

JASPListControl::LabelValueMap JASPListControl::_readValues(const QVariant& values)
{
	LabelValueMap result;

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
			QString textRole = property("textRole").toString();
			QString valueRole = property("valueRole").toString();
			if (textRole.isEmpty()) textRole = "label";
			if (valueRole.isEmpty()) valueRole = "value";


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
					QString label = labelValuePair[textRole].toString();
					QString value = labelValuePair[valueRole].toString();
					result.push_back(std::make_pair(label, value));
				}
			}
		}
	}

	return result;
}

void JASPListControl::setupSources()
{
	for (SourceType* sourceType : _sourceModels)
	{
		if (sourceType->isValuesSource)
			delete sourceType->model; // In case of values, the model is created just to contain the values, and does not come from another listview.
		delete sourceType;
	}
	_sourceModels.clear();

	if ((!source().isValid() || source().isNull()) && (!values().isValid() || values().isNull()))
		return;

	if (values().isValid() && !values().isNull())
		_sourceModels.append(new SourceType(_readValues(values())));

	QList<QVariant> sources = _getListVariant(source());
	
	for (const QVariant& source : sources)
	{
		QString sourceName, sourceControl, sourceUse;
		LabelValueMap sourceValues;
		QMap<QString, QVariant> map = _readSource(source, sourceName, sourceControl, sourceUse, sourceValues);

		QString conditionExpression = map["condition"].toString();
		QVector<std::tuple<QString, QString, QString, LabelValueMap, bool> > discards;
		QVector<QMap<QString, QVariant> > conditionVariables;
		bool combineWithOtherModels = false;

		if (sourceName.isEmpty() && !map.contains("values"))
		{
			addControlError(tr("No name given in source attribute of List %1").arg(name()));
			continue;
		}

		if (map.contains("discard"))
		{
			QList<QVariant> discardSources = _getListVariant(map["discard"]);

			for (const QVariant& discardSource : discardSources)
			{
				QString discardName, discardControl, discardUse;
				LabelValueMap discardValues;
				QMap<QString, QVariant> discardMap = _readSource(discardSource, discardName, discardControl, discardUse, discardValues);

				if (discardName.isEmpty())
					addControlError(tr("No name given in discard source attribute of VariableList %1" ).arg(name()));

				discards.push_back(std::make_tuple(discardName, discardControl, discardUse, discardValues, discardMap.contains("values")));
			}
		}

		if (map.contains("conditionVariables"))
		{
			QList<QVariant> conditionVariablesList = _getListVariant(map["conditionVariables"]);

			for (const QVariant& conditionVariablesVar : conditionVariablesList)
				if (conditionVariablesVar.canConvert<QMap<QString, QVariant> >())
					conditionVariables.push_back(conditionVariablesVar.toMap());
		}

		if (map.contains("combineWithOtherModels"))
			combineWithOtherModels = map["combineWithOtherModels"].toBool();

		bool isValuesSource = map.contains("values");
		_sourceModels.append(new SourceType(sourceName, sourceControl, sourceUse, sourceValues, isValuesSource, discards, conditionExpression, conditionVariables, combineWithOtherModels));
	}
	
	ListModel* listModel = model();

	if (_sourceModels.isEmpty())
	{
		if (_needsSourceModels)
			addControlError(tr("Needs source model for component %1").arg(name()));
	}
	else
	{
		bool termsAreVariables = true;
		bool termsAreInteractions = false;
		for (SourceType* sourceItem : _sourceModels)
		{
			ListModel* sourceModel = nullptr;
			if (!sourceItem->isValuesSource)
			{
				if (form())
					sourceModel = form()->getModel(sourceItem->name);
				else
					Log::log() << "Cannot use a source property outside of an Anlysis Form" << std::endl;
			}
			else
				sourceModel = new ListModelLabelValueTerms(this, sourceItem->values);

			if (sourceModel)
			{
				if (!sourceModel->areTermsVariables() || !sourceItem->controlName.isEmpty() || sourceItem->modelUse == "levels")
					termsAreVariables = false;
				if (sourceModel->areTermsInteractions() || sourceItem->combineWithOtherModels)
					termsAreInteractions = true;
				sourceItem->model = sourceModel;
				addDependency(sourceModel->listView());
				connect(sourceModel, &ListModel::termsChanged, listModel, &ListModel::sourceTermsChanged);

				for (SourceType& discardSource : sourceItem->discardModels)
				{
					ListModel* discardModel = nullptr;
					if (!sourceItem->isValuesSource)
					{
						if (form())
							discardModel = form()->getModel(discardSource.name);
						else
							Log::log() << "Cannot use a source property outside of an Anlysis Form" << std::endl;
					}

					else
						discardModel = new ListModelLabelValueTerms(this, discardSource.values);

					if (discardModel)
					{
						discardSource.model = discardModel;
						addDependency(discardModel->listView());
						connect(discardModel, &ListModel::termsChanged, listModel, &ListModel::sourceTermsChanged);
					}
					else
						addControlError(tr("Unknown discard model %1 for VariableList %2").arg(discardSource.name).arg(name()));
				}
			}
			else
				addControlError(tr("Cannot find source %1 for VariablesList %2").arg(sourceItem->name).arg(name()));
		}

		if (!termsAreVariables)
			listModel->setTermsAreVariables(false); // set it only when it is false
		if (termsAreInteractions)
			listModel->setTermsAreInteractions(true); // set it only when it is true
	}

}

void JASPListControl::addRowComponentsDefaultOptions(Options *options)
{
	if (!hasRowComponent())
		return;

	if (_defaultRowControls)
		delete _defaultRowControls;

	// Create a dummy QML control, so that we can create the right kind of options.
	_defaultRowControls = new RowControls(this->model(), rowComponent(), QMap<QString, Option*>(), true);
	_defaultRowControls->init(0, Term(_defaultKey), true);

	const QMap<QString, JASPControl*>& map = _defaultRowControls->getJASPControlsMap();
	QMapIterator<QString, JASPControl*> it(map);

	while (it.hasNext())
	{
		it.next();
		JASPControl* control = it.value();
		BoundControl* boundItem = dynamic_cast<BoundControl*>(control);
		if (boundItem)
		{
			// The options might depend on properties set by the setup
			// e.g. setup of BoundQMLListViewTerms sets whether the terms have interactions, which influences the kind of options that will be used.
			control->setUp();
			Option* option = boundItem->createOption();
			std::string optionName = control->name().toStdString();

			if (form() && (optionName == _optionKey.toStdString()))
				form()->addFormError(tr("The list %1 has a rowComponent with the same name (%2) as its optionKey. Change the optionKey property of the list or the control name.").arg(name()).arg(tq(optionName)));
			options->add(optionName, option);
		}
	}
}

void JASPListControl::setUp()
{
	if (!model())	setUpModel();
	JASPControl::setUp();
	_setAllowedVariables();

	ListModel* listModel = model();
	if (!listModel)	return;

	listModel->setRowComponent(rowComponent());
	setupSources();

	connect(this,		&JASPListControl::sourceChanged,	this,	&JASPListControl::sourceChangedHandler);
	connect(listModel,	&ListModel::termsChanged,			this,	&JASPListControl::termsChangedHandler);
	connect(listModel,	&ListModel::termsChanged,			[this]() { emit countChanged(); });
}

void JASPListControl::cleanUp()
{
	ListModel* _model = model();
	if (_model)
		_model->disconnect();
	JASPControl::cleanUp();
}

QList<std::pair<JASPListControl::SourceType*, Terms> > JASPListControl::getTermsPerSource()
{
	QList<std::pair<SourceType*, Terms> > result; // Do not use a map in order to keep the order of the sources.

	for (SourceType* sourceItem : _sourceModels)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			Terms sourceTerms = sourceModel->terms(sourceItem->modelUse);

			for (const SourceType& discardModel : sourceItem->getDiscardModels())
				sourceTerms.discardWhatDoesContainTheseComponents(discardModel.model->terms(discardModel.modelUse));

			if (!sourceItem->conditionExpression.isEmpty())
			{
				Terms filteredTerms;
				QJSEngine* jsEngine = qmlEngine(this);

				for (const Term& term : sourceTerms)
				{
					for (const SourceType::ConditionVariable& conditionVariable : sourceItem->conditionVariables)
					{
						JASPControl* control = sourceModel->getRowControl(term.asQString(), conditionVariable.controlName);
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

					QJSValue result = jsEngine->evaluate(sourceItem->conditionExpression);
					if (result.isError())
							addControlError("Error when evaluating : " + sourceItem->conditionExpression + ": " + result.toString());
					else if (result.toBool())
						filteredTerms.add(term);
				}

				sourceTerms = filteredTerms;
			}

			result.append(std::make_pair(sourceItem, sourceTerms));
		}
	}

	return result;
}

bool JASPListControl::addRowControl(const QString &key, JASPControl *control)
{
	bool success = false;

	if (key == _defaultKey)
	{
		if (_defaultRowControls)
			success = _defaultRowControls->addJASPControl(control);
	}
	else
		success = model()->addRowControl(key, control);

	return success;
}

bool JASPListControl::hasRowComponent() const
{
	return rowComponent() != nullptr;
}

JASPControl *JASPListControl::getChildControl(QString key, QString name)
{
	return getRowControl(key, name);
}

JASPControl *JASPListControl::getRowControl(const QString &key, const QString &name) const
{
	return model()->getRowControl(key, name);
}

QString JASPListControl::getSourceType(QString name)
{
	return model()->getItemType(name);
}

int JASPListControl::count()
{
	return model() ? model()->rowCount() : 0;
}

void JASPListControl::sourceChangedHandler()
{
	ListModel* listModel = model();
	if (!listModel)
		return;

	for (SourceType* sourceItem : _sourceModels)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			removeDependency(sourceModel->listView());
			disconnect(sourceModel, &ListModel::termsChanged, listModel, &ListModel::sourceTermsChanged);
			for (SourceType& discardModel : sourceItem->getDiscardModels())
				disconnect(discardModel.model, &ListModel::termsChanged, listModel, &ListModel::sourceTermsChanged);
		}
	}

	setupSources();
	listModel->sourceTermsChanged(nullptr, nullptr);
}

int JASPListControl::_getAllowedColumnsTypes()
{
	int allowedColumnsTypes = -1;
	
	QStringList allowedColumns = property("allowedColumns").toStringList();
	if (allowedColumns.isEmpty())
	{
		QString allowedColumn = property("allowedColumns").toString();
		if (!allowedColumn.isEmpty())
			allowedColumns.append(allowedColumn);
	}
	if (!allowedColumns.isEmpty())
	{
		allowedColumnsTypes = 0;
		for (QString& allowedColumn: allowedColumns)
		{
			if (allowedColumn == "ordinal")				allowedColumnsTypes |= int(columnType::ordinal);
			else if (allowedColumn == "nominal")		allowedColumnsTypes |= int(columnType::nominal);
			else if (allowedColumn == "nominalText")	allowedColumnsTypes |= int(columnType::nominalText);
			else if (allowedColumn == "scale")			allowedColumnsTypes |= int(columnType::scale);
			else
				addControlError(tr("Wrong column type: %1 for ListView %2").arg(allowedColumn).arg(name()));
		}
	}
	
	return allowedColumnsTypes;
}

void JASPListControl::_setAllowedVariables()
{
	_variableTypesAllowed = 0xff;
	
	int allowedColumnsTypes = _getAllowedColumnsTypes();
	
	if (allowedColumnsTypes >= 0)
		_variableTypesAllowed = allowedColumnsTypes;
}

JASPListControl::SourceType::SourceType(
		  const QString& _name
		, const QString& _controlName
		, const QString& _modelUse
		, const LabelValueMap& _values
		, bool _isValuesSource
		, const QVector<std::tuple<QString, QString, QString, LabelValueMap, bool> >& _discardModels
		, const QString& _conditionExpression
		, const QVector<QMap<QString, QVariant> >& _conditionVariables
		, bool _combineWithOtherModels
		)
	: name(_name), controlName(_controlName), modelUse(_modelUse), values(_values), isValuesSource(_isValuesSource), model(nullptr), conditionExpression(_conditionExpression), combineWithOtherModels(_combineWithOtherModels)
{
	if (!_controlName.isEmpty()) usedControls.insert(_controlName);

	for (const std::tuple<QString, QString, QString, LabelValueMap, bool>& discardModel : _discardModels)
	{
		discardModels.push_back(SourceType(std::get<0>(discardModel), std::get<1>(discardModel), std::get<2>(discardModel), std::get<3>(discardModel), std::get<4>(discardModel)));
		if (!std::get<1>(discardModel).isEmpty()) usedControls.insert(std::get<1>(discardModel));
	}

	for (const QMap<QString, QVariant>& conditionVariable : _conditionVariables)
	{
		conditionVariables.push_back(ConditionVariable(conditionVariable["name"].toString()
									, conditionVariable["component"].toString()
									, conditionVariable["property"].toString()
									, conditionVariable["addQuotes"].toBool())
					);
		if (!conditionVariable["component"].toString().isEmpty()) usedControls.insert(conditionVariable["component"].toString());
	}
}

QVector<JASPListControl::SourceType> JASPListControl::SourceType::getDiscardModels(bool onlyNotNullModel) const
{
	if (!onlyNotNullModel)
		return discardModels;

	QVector<JASPListControl::SourceType> result;

	for (const JASPListControl::SourceType& discardModel : discardModels)
		if (discardModel.model)
			result.push_back(discardModel);

	return result;
}
