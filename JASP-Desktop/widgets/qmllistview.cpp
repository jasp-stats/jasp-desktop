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

void QMLListView::setSources()
{
	_sourceModels.clear();

	QVariant sourcesVar = getItemProperty("source");
	if (sourcesVar.isNull())
		return;

	QList<QVariant> sources = _getListVariant(sourcesVar);
	
	for (const QVariant& source : sources)
	{
		JASPControlBase* sourceItem = source.value<JASPControlBase*>();
		if (sourceItem)
			_sourceModels.append(new SourceType(sourceItem->name()));
		else if (source.canConvert<QString>())
			_sourceModels.append(new SourceType(source.toString()));
		else if (source.canConvert<QMap<QString, QVariant> >())
		{
			QMap<QString, QVariant> map = source.toMap();
			QString sourceName;
			if (map.contains("id"))
			{
				JASPControlBase* sourceItem2 = map["id"].value<JASPControlBase*>();
				if (sourceItem2)
					sourceName = sourceItem2->name();
			}
			else
				sourceName = map["name"].toString();
			QString modelUse = map["use"].toString();
			QString conditionExpression = map["condition"].toString();
			QVector<QPair<QString, QString> > discards;
			QVector<QMap<QString, QVariant> > conditionVariables;

			if (sourceName.isEmpty())
			{
				addControlError(tr("No name given in source attribute of VariableList %1").arg(name()));
				continue;
			}

			if (map.contains("discard"))
			{
				QList<QVariant> discardSources = _getListVariant(map["discard"]);

				for (const QVariant& discardSource : discardSources)
				{
					QString discardName;
					QString discardUse;

					JASPControlBase* discardItem = source.value<JASPControlBase*>();
					if (discardItem)
						discardName = discardItem->name();
					else if (discardSource.canConvert<QString>())
						discardName = discardSource.toString();
					else if (discardSource.canConvert<QMap<QString, QVariant> >())
					{
						QMap<QString, QVariant> discardMap = discardSource.toMap();
						if (discardMap.contains("id"))
						{
							JASPControlBase* discardItem2 = map["id"].value<JASPControlBase*>();
							if (discardItem2)
								discardName = discardItem2->name();
						}
						else
							discardName = discardMap["name"].toString();
						if (discardName.isEmpty())
							addControlError(tr("No name given in discard source attribute of VariableList %1" ).arg(name()));
						discardUse = discardMap["use"].toString();
					}
					else
						addControlError(tr("Wrong parameter discard in VariablesList %1").arg(name()));
					discards.push_back(QPair<QString, QString>(discardName, discardUse));
				}
			}

			if (map.contains("conditionVariables"))
			{
				QList<QVariant> conditionVariablesList = _getListVariant(map["conditionVariables"]);

				for (const QVariant& conditionVariablesVar : conditionVariablesList)
				{
					if (conditionVariablesVar.canConvert<QMap<QString, QVariant> >())
						conditionVariables.push_back(conditionVariablesVar.toMap());
				}
			}

			_sourceModels.append(new SourceType(sourceName, modelUse, discards, conditionExpression, conditionVariables));
		}
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
				if (!sourceModel->areTermsVariables())
					termsAreVariables = false;
				if (sourceModel->areTermsInteractions())
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
			Option* option = boundItem->createOption();
			options->add(boundItem->name().toStdString(), option);
		}
	}
}

void QMLListView::readModelProperty(QMap<QString, QString>* labelToValueMap)
{
	QVariant modelVar = getItemProperty("values");

	if (modelVar.isNull())
	{
		if (getItemProperty("source").isNull())
			_modelHasAllVariables = true;
	}
	else
	{
		QString textRole = getItemProperty("textRole").toString();
		QString valueRole = getItemProperty("valueRole").toString();
		Terms terms;
		QList<QVariant> list = modelVar.toList();
		if (!list.isEmpty())
		{
			for (const QVariant& itemVariant : list)
			{
				QMap<QString, QVariant> labelValuePair = itemVariant.toMap();
				if (labelValuePair.isEmpty())
					terms.add(itemVariant.toString());
				else
				{
					QString label = labelValuePair[textRole].toString();
					QString value = labelValuePair[valueRole].toString();
					terms.add(label);
					if (labelToValueMap)
						(*labelToValueMap)[label] = value;
				}
			}
			model()->initTerms(terms);
		}
		else
		{
			QAbstractItemModel *srcModel = qobject_cast<QAbstractItemModel *>(modelVar.value<QObject *>());
			if (srcModel)
			{
				QMap<QString, int> roleMap;
				QHash<int, QByteArray> roles = srcModel->roleNames();
				QHashIterator<int, QByteArray> i(roles);
				while (i.hasNext())
				{
					i.next();
					QString valueStr = QString::fromStdString(i.value().toStdString());
					roleMap[valueStr] = i.key();
				}
				for (int i = 0; i < srcModel->rowCount(); i++)
				{
					QModelIndex ind(srcModel->index(i, 0));
					QString label = srcModel->data(ind, roleMap[textRole]).toString();
					QString value = srcModel->data(ind, roleMap[valueRole]).toString();
					terms.add(label);
					if (labelToValueMap)
						(*labelToValueMap)[label] = value;
				}
				model()->initTerms(terms);
			}
			else
				model()->initTerms(Terms());
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

QMLListView::SourceType* QMLListView::getSourceTypeFromModel(ListModel* model)
{
	QMLListView::SourceType* result = nullptr;
	const QList<QMLListView::SourceType*>& sourceTypes = sourceModels();

	for (QMLListView::SourceType* sourceType : sourceTypes)
	{
		if (sourceType->model == model)
			result = sourceType;
	}

	return result;
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
