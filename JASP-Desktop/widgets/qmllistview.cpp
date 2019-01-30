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

#include <QQmlProperty>
#include <QDebug>

QMLListView::QMLListView(QQuickItem *item, AnalysisForm *form) 
	: QObject(form)
	, _needsSourceModels(false)
	  
{
	_setAllowedVariables();	
	_sourceModelsList = QQmlProperty(_item, "source").read().toStringList();	
}

void QMLListView::setUp()
{
	QMLItem::setUp();
	
	ListModel* listModel = model();
	if (!listModel)
		return;
	
	if (_sourceModelsList.isEmpty())
	{
		if (_needsSourceModels)
			addError(QString::fromLatin1("Needs source model for VariablesList ") + name());
	}
	else
	{
		bool areTermsVariables = true;
		for (const QString& sourceModelName : _sourceModelsList)
		{
			ListModel* sourceModel = _form->getModel(sourceModelName);
			if (sourceModel)
			{
				if (!sourceModel->areTermsVariables())
					areTermsVariables = false;
				_sourceModels.push_back(sourceModel);
				addDependency(sourceModel->listView());
				connect(sourceModel, &ListModel::modelChanged, listModel, &ListModel::sourceTermsChanged);
			}
			else
				addError(QString::fromLatin1("Unknown source model ") + sourceModelName + QString::fromLatin1(" for ModelVIew ") + name());
		}
		
		if (!areTermsVariables)
			setTermsAreNotVariables(); // set it only when it is false
	}

	connect(listModel, &ListModel::modelChanged, this, &QMLListView::modelChangedHandler);
	QQmlProperty(_item, "model").write(QVariant::fromValue(listModel));
}

void QMLListView::setTermsAreNotVariables()
{
	model()->setTermsAreVariables(false);
	QQmlProperty::write(_item, "showVariableTypeIcon", false);
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
