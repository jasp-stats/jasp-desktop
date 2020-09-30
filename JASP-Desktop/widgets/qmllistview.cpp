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

#include "qmllistview.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrolbase.h"
#include "listmodel.h"
#include "log.h"
#include "rowcontrols.h"

#include <QQmlContext>

const QString QMLListView::_defaultKey = "_JASPDefaultKey";


QMLListView::QMLListView(JASPControlBase *item)
	: QObject(item)
	, _needsSourceModels(false)
	  
{
	_hasSource = !getItemProperty("source").isNull(); // sourceModels are only known after setup is called, but the method hasSource() is needed before in AnalysisForm
	_hasRowComponents = item->rowComponentsCount() > 0;
	_optionKeyName = getItemProperty("optionKey").toString().toStdString();
}

QList<QVariant> QMLListView::_getListVariant(QVariant var)
{
	QList<QVariant> listVar = var.toList();

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

QString QMLListView::_readSourceName(const QString& sourceNameExt, QString& sourceControl, QString& sourceUse)
{
	QStringList nameSplit = sourceNameExt.split(".");
	if (nameSplit.length() > 1)
	{
		sourceControl = nameSplit[1];
		sourceUse = "control=" + nameSplit[1];
	}

	return nameSplit[0];
}

QMap<QString, QVariant> QMLListView::_readSource(const QVariant& source, QString& sourceName, QString& sourceControl, QString& sourceUse)
{
	QMap<QString, QVariant> map;

	JASPControlBase* sourceItem = source.value<JASPControlBase*>();
	if (sourceItem)
		sourceName = sourceItem->name();
	else if (source.type() == QVariant::Type::String)
		sourceName = _readSourceName(source.toString(), sourceControl, sourceUse);
	else if (source.canConvert<QMap<QString, QVariant> >())
	{
		map = source.toMap();
		if (map.contains("id"))
		{
			JASPControlBase* sourceItem2 = map["id"].value<JASPControlBase*>();
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
	}

	return map;
}

void QMLListView::setSources()
{
	_sourceModels.clear();

	QVariant sourcesVar = getItemProperty("source");
	if (sourcesVar.isNull())
		return;

	QList<QVariant> sources = _getListVariant(sourcesVar);
	
	for (const QVariant& source : sources)
	{
		QString sourceName, sourceControl, sourceUse;
		QMap<QString, QVariant> map = _readSource(source, sourceName, sourceControl, sourceUse);

		QString conditionExpression = map["condition"].toString();
		QVector<std::tuple<QString, QString, QString> > discards;
		QVector<QMap<QString, QVariant> > conditionVariables;
		bool combineWithOtherModels = false;

		if (sourceName.isEmpty())
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
				_readSource(discardSource, discardName, discardControl, discardUse);

				if (discardName.isEmpty())
					addControlError(tr("No name given in discard source attribute of VariableList %1" ).arg(name()));

				discards.push_back(std::make_tuple(discardName, discardControl, discardUse));
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

		_sourceModels.append(new SourceType(sourceName, sourceControl, sourceUse, discards, conditionExpression, conditionVariables, combineWithOtherModels));
	}
	
	ListModel* listModel = model();

	if (_sourceModels.isEmpty())
	{
		if (_needsSourceModels)
			addControlError(tr("Needs source model for VariablesList %1").arg(name()));
	}
	else
	{
		bool termsAreVariables = true;
		bool termsAreInteractions = false;
		for (SourceType* sourceItem : _sourceModels)
		{
			ListModel* sourceModel = form()->getModel(sourceItem->name);
			if (sourceModel)
			{
				if (!sourceModel->areTermsVariables() || !sourceItem->controlName.isEmpty() || sourceItem->modelUse == "levels")
					termsAreVariables = false;
				if (sourceModel->areTermsInteractions() || sourceItem->combineWithOtherModels)
					termsAreInteractions = true;
				sourceItem->model = sourceModel;
				addDependency(sourceModel->listView());
				connect(sourceModel, &ListModel::modelChanged, listModel, &ListModel::sourceTermsChanged);

				for (SourceType& discardSource : sourceItem->discardModels)
				{
					ListModel* discardModel = form()->getModel(discardSource.name);
					if (discardModel)
					{
						discardSource.model = discardModel;
						addDependency(discardModel->listView());
						connect(discardModel, &ListModel::modelChanged, listModel, &ListModel::sourceTermsChanged);
					}
					else
						addControlError(tr("Unknown discard model %1 for VariableList %2").arg(discardSource.name).arg(name()));
				}
			}
			else
				addControlError(tr("Cannot find source %1 for VariablesList %2").arg(sourceItem->name).arg(name()));
		}

		if (!termsAreVariables)
			setTermsAreNotVariables(); // set it only when it is false
		if (termsAreInteractions)
			setTermsAreInteractions(); // set it only when it is true

		if (_sourceModels.length() == 1 && _sourceModels[0]->model)
			setItemProperty("sourceModel", QVariant::fromValue(_sourceModels[0]->model));
	}

}

void QMLListView::addRowComponentsDefaultOptions(Options *options)
{
	if (!_hasRowComponents)
		return;

	if (_defaultRowControls)
		delete _defaultRowControls;

	// Create a dummy QML control, so that we can create the right kind of options.
	_defaultRowControls = new RowControls(this->model(), item()->getRowComponents(), QMap<QString, Option*>(), true);
	_defaultRowControls->init(0, Term(_defaultKey), true);

	const QMap<QString, JASPControlWrapper*>& map = _defaultRowControls->getJASPControlsMap();
	QMapIterator<QString, JASPControlWrapper*> it(map);

	while (it.hasNext())
	{
		it.next();
		JASPControlWrapper* wrapper = it.value();
		BoundQMLItem* boundItem = dynamic_cast<BoundQMLItem*>(wrapper);
		if (boundItem)
		{
			// The options might depend on properties set by the setup
			// e.g. setup of BoundQMLListViewTerms sets whether the terms have interactions, which influences the kind of options that will be used.
			boundItem->setUp();
			Option* option = boundItem->createOption();
			std::string optionName = boundItem->name().toStdString();

			if (form() && (optionName == _optionKeyName))
				form()->addFormError(tr("The list %1 has a rowComponent with the same name (%2) as its optionKey. Change the optionKey property of the list or the control name.").arg(name()).arg(tq(optionName)));
			options->add(optionName, option);
		}
	}
}

void QMLListView::setUp()
{
	JASPControlWrapper::setUp();
	_setAllowedVariables();

	ListModel* listModel = model();
	if (!listModel)
		return;

	listModel->setRowComponents(item()->getRowComponents());
	setSources();

	if (!getItemProperty("source").isNull())
		QQuickItem::connect(_item, SIGNAL(sourceChanged()), this, SLOT(sourceChangedHandler()));

	connect(listModel, &ListModel::modelChanged, this, &QMLListView::modelChangedHandler);

	setItemProperty("model", QVariant::fromValue(listModel));	
}

void QMLListView::cleanUp()
{
	ListModel* _model = model();
	if (_model)
		_model->disconnect();
	JASPControlWrapper::cleanUp();
}

void QMLListView::setTermsAreNotVariables()
{
	model()->setTermsAreVariables(false);
	setItemProperty("showVariableTypeIcon", false);
}

void QMLListView::setTermsAreInteractions()
{
	model()->setTermsAreInteractions(true);
}

bool QMLListView::addRowControl(const QString &key, JASPControlWrapper *control)
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

JASPControlWrapper *QMLListView::getChildControl(QString key, QString name)
{
	return getRowControl(key, name);
}

JASPControlWrapper *QMLListView::getRowControl(const QString &key, const QString &name) const
{
	return model()->getRowControl(key, name);
}

QString QMLListView::getSourceType(QString name)
{
	return model()->getItemType(name);
}

void QMLListView::sourceChangedHandler()
{
	if (getItemProperty("source").isNull())
		return;

	ListModel* listModel = model();
	if (!listModel)
		return;

	for (SourceType* sourceItem : _sourceModels)
	{
		ListModel* sourceModel = sourceItem->model;
		if (sourceModel)
		{
			removeDependency(sourceModel->listView());
			disconnect(sourceModel, &ListModel::modelChanged, listModel, &ListModel::sourceTermsChanged);
			for (SourceType& discardModel : sourceItem->getDiscardModels())
				disconnect(discardModel.model, &ListModel::modelChanged, listModel, &ListModel::sourceTermsChanged);
		}
	}

	setSources();
	listModel->sourceTermsChanged(nullptr, nullptr);
}

int QMLListView::_getAllowedColumnsTypes()
{
	int allowedColumnsTypes = -1;
	
	QStringList allowedColumns = getItemProperty("allowedColumns").toStringList();
	if (allowedColumns.isEmpty())
	{
		QString allowedColumn = getItemProperty("allowedColumns").toString();
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

void QMLListView::_setAllowedVariables()
{
	_variableTypesAllowed = 0xff;
	
	int allowedColumnsTypes = _getAllowedColumnsTypes();
	
	if (allowedColumnsTypes >= 0)
		_variableTypesAllowed = allowedColumnsTypes;
}

QMLListView::SourceType::SourceType(
		  const QString& _name
		, const QString& _controlName
		, const QString& _modelUse
		, const QVector<std::tuple<QString, QString, QString> >& _discardModels
		, const QString& _conditionExpression
		, const QVector<QMap<QString, QVariant> >& _conditionVariables
		, bool _combineWithOtherModels)
	: name(_name), controlName(_controlName), modelUse(_modelUse), model(nullptr), conditionExpression(_conditionExpression), combineWithOtherModels(_combineWithOtherModels)
{
	if (!_controlName.isEmpty()) usedControls.insert(_controlName);

	for (const std::tuple<QString, QString, QString>& discardModel : _discardModels)
	{
		discardModels.push_back(SourceType(std::get<0>(discardModel), std::get<1>(discardModel), std::get<2>(discardModel)));
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

QVector<QMLListView::SourceType> QMLListView::SourceType::getDiscardModels(bool onlyNotNullModel) const
{
	if (!onlyNotNullModel)
		return discardModels;

	QVector<QMLListView::SourceType> result;

	for (const QMLListView::SourceType& discardModel : discardModels)
		if (discardModel.model)
			result.push_back(discardModel);

	return result;
}
