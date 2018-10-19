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

#include "boundqmllistviewdraggable.h"
#include "analysis/analysisqmlform.h"
#include "listmodelassignedinterface.h"
#include "qmllistviewtermsavailable.h"
#include "analysis/options/optionstable.h"
#include "analysis/options/optionvariable.h"

#include "boundqmlcheckbox.h"
#include "boundqmlcombobox.h"
#include "boundqmltextinput.h"

#include <QQuickItem>
#include <QQmlProperty>
#include <QDebug>

BoundQMLListViewDraggable::BoundQMLListViewDraggable(QQuickItem *item, AnalysisQMLForm *form)
	: QMLListViewDraggable(item, form)
	, BoundQMLItem(item, form) 
{
	_hasExtraControlColumns = QQmlProperty(_item, "hasExtraControlColumns").read().toBool();
	if (_hasExtraControlColumns)
	{
		QList<QVariant> extraColumns = QQmlProperty(_item, "controlColumns").read().toList();
		for (const QVariant& extraColumnVariant : extraColumns)
		{
			QMap<QString, QString> properties;
			QString columnName;
			
			QObject* extraColumnObject = extraColumnVariant.value<QObject*>();
			const QMetaObject *meta = extraColumnObject->metaObject();
			int propertyCount = meta->propertyCount();
			for (int i = 0; i < propertyCount; ++i)
			{
				QMetaProperty property = meta->property(i);
				QString key = QString::fromLatin1(property.name());
				QString value = property.read(extraColumnObject).toString();
				if (key == "name")
					columnName = value;
				else
					properties[key] = value;
			}
					
			if (columnName.isEmpty())
				addError(QString::fromLatin1("An Extra column in ") + name() + QString::fromLatin1(" has no name"));
			else
				_extraControlColumns[columnName] = properties;
		}
		
	}
}

void BoundQMLListViewDraggable::setUp()
{	
	QMLListViewDraggable::setUp();
	
	if (_hasExtraControlColumns)
	{
		// The extra controls are handled completly outside the model
		// The QML VariableList adds automatically the controls dynamically
		// This class had to build the right Bound objects and associate them with each control
		// The concrete classes (BoundQMLListViewAnovaModels and BoundQMLListViewTerms) will have to bind the options with the bound objects.
		QQuickItem::connect(_item, SIGNAL(removeRowWithControls(QString)), this, SLOT(removeRowWithControlsHandler(QString)));
		QQuickItem::connect(_item, SIGNAL(addRowWithControls(QString, QVariant)), this, SLOT(addRowWithControlsHandler(QString, QVariant)));	
	}
	ListModel* sourceModel = _form->getRelatedModel(this);
	if (!sourceModel)
	{
		if (syncModels().empty())
			addError(QString::fromLatin1("Cannot find source ListView for item ") + name());
	}
	else
	{
		_sourceModel = dynamic_cast<ListModelAvailableInterface*>(sourceModel);
		ListModelAssignedInterface* _assignedModel = assignedModel();
		if (!_sourceModel)
			addError(QString::fromLatin1("Wrong kind of source ListView for item ") + name());
		else
		{
			_assignedModel->setSource(_sourceModel);
			QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(_sourceModel->listView());
			if (qmlAvailableListView)
				qmlAvailableListView->addAssignedModel(_assignedModel);
			connect(_sourceModel, &ListModelAvailableInterface::modelChanged, _assignedModel, &ListModelAssignedInterface::availableTermsChanged);			
		}
	}
}

ListModelAssignedInterface* BoundQMLListViewDraggable::assignedModel()
{
	return dynamic_cast<ListModelAssignedInterface*>(model());
}


void BoundQMLListViewDraggable::removeRowWithControlsHandler(QString termName)
{
	if (_rowsWithControls.contains(termName))
	{
		_cachedRowsWithControls[termName] = _rowsWithControls[termName];
		_rowsWithControls.remove(termName);
	}
	else
		qDebug() << termName + " is unknown!!!";	
}

void BoundQMLListViewDraggable::addRowWithControlsHandler(QString termName, QVariant controls)
{
	QList<QVariant> controlList = controls.toList();
	QList<QQuickItem*> controlItems;
	
	for (const QVariant& controlVariant : controlList)
	{
		QQuickItem* controlItem = qvariant_cast<QQuickItem *>(controlVariant);
		controlItems.push_back(controlItem);
	}
	
	QMap<QString, BoundQMLItem *> row;
	if (_cachedRowsWithControls.contains(termName))
	{
		row = _cachedRowsWithControls[termName];
		QList<BoundQMLItem*> boundItems = row.values();
		int i = 0;
		for (QQuickItem* controlItem : controlItems)
		{
			if (i >= boundItems.size())
			{
				qDebug() << "Cached Row has only " << boundItems.size() << " item(s) but QML has " << controlItems.length() << " control(s)!!!!";
				break;
			}
			BoundQMLItem* boundItem = boundItems[i];
			boundItem->resetQMLItem(controlItem);
			i++;
		}
	}
	else
	{
		for (QQuickItem* controlItem : controlItems)
		{
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
			case qmlControlType::Switch:		boundQMLItem = new BoundQMLCheckBox(controlItem,	form());	break;
			case qmlControlType::TextField:		boundQMLItem = new BoundQMLTextInput(controlItem,	form());	break;
			case qmlControlType::ComboBox:		boundQMLItem = new BoundQMLComboBox(controlItem,	form());	break;
			default:
				qDebug() << "Control type " << controlTypeStr << " not supported in TableView";
			}
			
			if (boundQMLItem)
			{
				boundQMLItem->setUp();
				row[boundQMLItem->name()] = boundQMLItem;
			}
		}
	}
	_rowsWithControls[termName] = row;
}
