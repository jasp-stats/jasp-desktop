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

#include <QQmlProperty>
#include <QDebug>

QMLListView::QMLListView(QQuickItem *item, AnalysisForm *form) 
	: QObject(form)
	, _needsSourceModels(false)
	  
{
	_setAllowedVariables();	
	QList<QVariant> sources = QQmlProperty(_item, "source").read().toList();
	
	if (sources.isEmpty())
	{
		QStringList stringSources = QQmlProperty(_item, "source").read().toStringList();
		for (const QString& stringSource : stringSources)
			sources.push_back(stringSource);
	}
	
	for (const QVariant& source : sources)
	{
		if (source.canConvert<QString>())
			_sourceModels.append(new SourceType(source.toString()));
		else if (source.canConvert<QMap<QString, QVariant> >())
		{
			QMap<QString, QVariant> map = source.toMap();
			QString sourceName = map["name"].toString();
			if (sourceName.isEmpty())
				addError("No name given is source attribute of VariableList " + name());
			else
			{
				QString discard = map["discard"].toString();
				_sourceModels.append(new SourceType(sourceName, discard));
			}
		}
	}
}

void QMLListView::setUp()
{
	QMLItem::setUp();
	
	ListModel* listModel = model();
	if (!listModel)
		return;
	
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
				
				if (!sourceItem->discard.isEmpty())
				{
					ListModel* discardModel = _form->getModel(sourceItem->discard);
					if (discardModel)
						sourceItem->discardModel = discardModel;
					else
						addError(QString::fromLatin1("Unknown discard model ") + sourceItem->discard + QString::fromLatin1(" for VariableList ") + name());
				}
			}
			else
				addError(QString::fromLatin1("Unknown source model ") + sourceItem->name + QString::fromLatin1(" for VariableList ") + name());
		}
		
		if (!termsAreVariables)
			setTermsAreNotVariables(); // set it only when it is false
		if (termsAreInteractions)
			setTermsAreInteractions(); // set it only when it is true
	}

	connect(listModel, &ListModel::modelChanged, this, &QMLListView::modelChangedHandler);
	QQmlProperty(_item, "model").write(QVariant::fromValue(listModel));
}

void QMLListView::setTermsAreNotVariables()
{
	model()->setTermsAreVariables(false);
	QQmlProperty::write(_item, "showVariableTypeIcon", false);
}

void QMLListView::setTermsAreInteractions()
{
	model()->setTermsAreInteractions(true);
}

int QMLListView::_getAllowedColumnsTypes()
{
	int allowedColumnsTypes = -1;
	
	QStringList allowedColumns = QQmlProperty(_item, "allowedColumns").read().toStringList();
	if (allowedColumns.isEmpty())
	{
		QString allowedColumn = QQmlProperty(_item, "allowedColumns").read().toString();
		if (!allowedColumn.isEmpty())
			allowedColumns.append(allowedColumn);
	}
	if (!allowedColumns.isEmpty())
	{
		allowedColumnsTypes = 0;
		for (QString& allowedColumn: allowedColumns)
		{
			if (allowedColumn == "ordinal")
				allowedColumnsTypes |= Column::ColumnTypeOrdinal;
			else if (allowedColumn == "nominal")
			{
				allowedColumnsTypes |= Column::ColumnTypeNominal;
				allowedColumnsTypes |= Column::ColumnTypeNominalText;
			}
			else if (allowedColumn == "nominalText")
				allowedColumnsTypes |= Column::ColumnTypeNominalText;
			else if (allowedColumn == "nominalInt")
				allowedColumnsTypes |= Column::ColumnTypeNominal;
			else if (allowedColumn == "scale")
				allowedColumnsTypes |= Column::ColumnTypeScale;
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
