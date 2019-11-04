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
#include "listmodel.h"
#include "interactionmodel.h"
#include "boundqmltextarea.h"
#include "log.h"
#include "extracontrolsinfo.h"

QMLListView::QMLListView(QQuickItem *item, AnalysisForm *form) 
	: QObject(form)
	, _needsSourceModels(false)
	  
{
	_extraControlsInfo.read(this);
	_hasExtraControls = _extraControlsInfo.values().length() > 0;

	_setAllowedVariables();
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
		if (source.canConvert<QString>())
			_sourceModels.append(new SourceType(source.toString()));
		else if (source.canConvert<QMap<QString, QVariant> >())
		{
			QMap<QString, QVariant> map = source.toMap();
			QString sourceName = map["name"].toString();
			QString modelUse = map["use"].toString();
			QVector<QPair<QString, QString> > discards;
			if (sourceName.isEmpty())
				addError("No name given in source attribute of VariableList " + name());
			else if (map.contains("discard"))
			{

				QList<QVariant> discardSources = _getListVariant(map["discard"]);

				for (const QVariant& discardSource : discardSources)
				{
					QString discardName;
					QString discardUse;
					if (discardSource.canConvert<QString>())
						discardName = discardSource.toString();
					else if (discardSource.canConvert<QMap<QString, QVariant> >())
					{
						QMap<QString, QVariant> discardMap = discardSource.toMap();
						discardName = discardMap["name"].toString();
						if (discardName.isEmpty())
							addError("No name given in discard source attribute of VariableList " + name());
						discardUse = discardMap["use"].toString();
					}
					else
						addError("Wrong parameter discard in VariablesList " + name());
					discards.push_back(QPair<QString, QString>(discardName, discardUse));
				}
				_sourceModels.append(new SourceType(sourceName, modelUse, discards));
			}
		}
	}
	
	ListModel* listModel = model();

	if (_sourceModels.isEmpty())
	{
		if (_needsSourceModels)
			addError(QString::fromLatin1("Needs source model for VariablesList ") + name());
	}
	else
	{
		bool termsAreVariables = true;
		bool termsAreInteractions = false;
		for (SourceType* sourceItem : _sourceModels)
		{
			ListModel* sourceModel = _form->getModel(sourceItem->name);
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
					ListModel* discardModel = _form->getModel(discardSource.name);
					if (discardModel)
					{
						discardSource.model = discardModel;
						connect(discardModel, &ListModel::modelChanged, listModel, &ListModel::sourceTermsChanged);
					}
					else
						addError(tr("Unknown discard model %1 for VariableList %2").arg(discardSource.name).arg(name()));
				}
			}
			else
				addError(tr("Cannot find source %1 for VariablesList %2").arg(sourceItem->name).arg(name()));
		}

		if (!termsAreVariables)
			setTermsAreNotVariables(); // set it only when it is false
		if (termsAreInteractions)
			setTermsAreInteractions(); // set it only when it is true
	}

}

void QMLListView::setUp()
{
	QMLItem::setUp();
	
	ListModel* listModel = model();
	if (!listModel)
		return;

	listModel->addExtraControls(_extraControlsInfo.values());
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
	QMLItem::cleanUp();
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
			for (SourceType& discardModel : sourceItem->discardModels)
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
				addError(QString::fromLatin1("Wrong column type: ") + allowedColumn + (_name.isEmpty() ? QString() : (QString::fromLatin1(" for ListView ") + _name)));
		}
	}
	
	return allowedColumnsTypes;
}

void QMLListView::_setAllowedVariables()
{
	_variableTypesSuggested = 0;
	_variableTypesAllowed = 0xff;
	
	int allowedColumnsTypes = _getAllowedColumnsTypes();
	
	if (allowedColumnsTypes >= 0)
		_variableTypesAllowed = allowedColumnsTypes;
}
