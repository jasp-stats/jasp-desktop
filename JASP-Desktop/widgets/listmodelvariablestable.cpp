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

#include "listmodelvariablestable.h"
#include "boundqmlcheckbox.h"
#include "boundqmlcombobox.h"
#include "boundqmltextinput.h"
#include "analysis/analysisqmldefines.h"
#include "analysis/analysisqmlform.h"

#include "analysis/options/optionvariable.h"

#include <QQmlProperty>
#include <QTimer>

ListModelVariablesTable::ListModelVariablesTable(AnalysisQMLForm *form, QQuickItem *item)
	: ListModel(form, item)
{
	QQuickItem::connect(item, SIGNAL(removeRow(QString)), this, SLOT(removeRowSlot(QString)));
	QQuickItem::connect(item, SIGNAL(addRow(QString, QVariant)), this, SLOT(addRowSlot(QString, QVariant)));	
}

int ListModelVariablesTable::rowCount(const QModelIndex &parent) const
{
	return _terms.size();
}

QVariant ListModelVariablesTable::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole || role == ListModelVariablesTable::NameRole)
	{
		Term term = _terms.at(row);
		return QVariant(term.asQString());		
	}
	
	return QVariant();
}

void ListModelVariablesTable::initTerms(const Terms &terms)
{
	beginResetModel();
	_terms.set(terms.terms());
	endResetModel();	
	
}

const QMap<QString, QList<BoundQMLItem *> > &ListModelVariablesTable::rows() const
{
	return _rows;
}

void ListModelVariablesTable::removeRowSlot(QString term)
{
	qDebug() << "Remove row " + term;
	if (_rows.contains(term))
	{
		_cachedRows[term] = _rows[term];
		_rows.remove(term);
	}
	else
		qDebug() << term + " is unknown!!!";
	
}

void ListModelVariablesTable::addRowSlot(QString term, QVariant controls)
{
	qDebug() << "Add row " << term  << " with: ";
	QList<QVariant> controlList = controls.toList();
	
	QList<BoundQMLItem *> row;
	if (_cachedRows.contains(term))
	{
		row = _cachedRows[term];
		int i = 0;
		for (const QVariant& controlVariant : controlList)
		{
			if (i >= row.length())
			{
				qDebug() << "Cached Row has only " << row.length() << " but QML has " << controlList.length() << " controls!!!!";
				break;
			}
			QQuickItem* controlItem = qvariant_cast<QQuickItem *>(controlVariant);
			BoundQMLItem* boundItem = row[i];
			boundItem->resetQMLItem(controlItem);
			i++;
		}
	}
	else
	{
		for (const QVariant& controlVariant : controlList)
		{
			QQuickItem* controlItem = qvariant_cast<QQuickItem *>(controlVariant);
			QString controlTypeStr = QQmlProperty(controlItem, "controlType").read().toString();
			if (controlTypeStr.isEmpty())
			{
				qDebug() << "Control Type undefined in TableView!!";
				continue;
			}
			
			BoundQMLItem* boundQMLItem = NULL;
			qmlControlType controlType = qmlControlTypeFromQString(controlTypeStr);
	
			switch(controlType)
			{
			case qmlControlType::CheckBox:		//fallthrough:
			case qmlControlType::Switch:		boundQMLItem = new BoundQMLCheckBox(controlItem,		_form);	break;
			case qmlControlType::TextField:		boundQMLItem = new BoundQMLTextInput(controlItem,		_form);	break;
			case qmlControlType::ComboBox:		boundQMLItem = new BoundQMLComboBox(controlItem,		_form);	break;
			default:
				addError("Control type " + controlTypeStr + " not supported in TableView");
			}
			
			if (boundQMLItem)
				row.append(boundQMLItem);
		}
	}
	_rows[term] = row;
}
