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
#include "analysis/AnalysisForm.h"
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

BoundQMLListViewDraggable::BoundQMLListViewDraggable(QQuickItem *item, AnalysisForm *form)
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
			else if (!properties.contains("type"))
				addError(QString::fromLatin1("An Extra column in ") + name() + QString::fromLatin1(" has no type"));
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
		QQuickItem::connect(_item, SIGNAL(removeRowWithControls(int, QString)), this, SLOT(removeRowWithControlsHandler(int, QString)));
		QQuickItem::connect(_item, SIGNAL(addRowWithControls(int, QString, QVariant)), this, SLOT(addRowWithControlsHandler(int, QString, QVariant)));	
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
			{
				qmlAvailableListView->addAssignedModel(_assignedModel);
				addDependency(qmlAvailableListView);
			}
			connect(_sourceModel, &ListModelAvailableInterface::modelChanged, _assignedModel, &ListModelAssignedInterface::availableTermsChanged);			
		}
	}
}

ListModelAssignedInterface* BoundQMLListViewDraggable::assignedModel()
{
	return dynamic_cast<ListModelAssignedInterface*>(model());
}

void BoundQMLListViewDraggable::addExtraOptions(Options *options)
{
	QMapIterator<QString, QMap<QString, QString> > it(_extraControlColumns);
	while (it.hasNext())
	{
		it.next();
		const QMap<QString, QString>& properties = it.value();
		QString type = properties["type"];
		Option* option = NULL;
		if (type == "CheckBox")
			option = new OptionBoolean();
		else if (type == "ComboBox")
			option = new OptionList(std::vector<std::string>());
		else if (type == "TextField")
			option = new OptionString();
		if (option)
			options->add(it.key().toStdString(), option);
		else
			addError(QString::fromLatin1("Extra column in ") + name() + " has an unsupported type: " + type);
	}
	
}

void BoundQMLListViewDraggable::removeRowWithControlsHandler(int index, QString name)
{	
	qDebug() << "Remove index " << index << ", name " << name;
	if (_rowsWithControls.contains(name))
	{
		_cachedRows.insert(name, qMakePair(index, _rowsWithControls[name]));
		_rowsWithControls.remove(name);
		_addedRows.remove(name);
	}
	else
		qDebug() << "removeRowWithControlsHandler: Row " << name << " is unknown!!!";	
}

void BoundQMLListViewDraggable::addRowWithControlsHandler(int index, QString name, QVariant controls)
{
	qDebug() << "Add index " << index << ", name " << name;
	QList<QVariant> controlList = controls.toList();
	QMap<QString, QQuickItem*> controlItems;
	
	for (const QVariant& controlVariant : controlList)
	{
		QQuickItem* controlItem = qvariant_cast<QQuickItem *>(controlVariant);
		QString controlName = controlItem->property("name").toString();
		if (controlName.isEmpty())
			addError(QString::fromLatin1("An Extra control Column in ") + this->name() + QString::fromLatin1(" has no name"));
		else
			controlItems[controlName] = controlItem;
	}

	_addedRows.insert(name, qMakePair(index, controlItems));

	// When all rows have been added in QML, map them with the right BoundItems
	if (model()->rowCount() == index + 1)
	{
		_rowsWithControls.clear();
		_mapRowsWithBoundItems();
		_cachedRows.clear();
		_addedRows.clear();
	}
}
	
void BoundQMLListViewDraggable::_mapRowsWithBoundItems()
{
	qDebug() << "Map rows with Bound Items";
	QMap<int, QPair<QString, QMap<QString, QQuickItem*> > > rowsNotFound;
	
	// First find the rows in the cache with the same name
	QMapIterator<QString, RowQuickItemType> addedRowsIt(_addedRows);	
	while(addedRowsIt.hasNext())
	{
		addedRowsIt.next();
		QString name = addedRowsIt.key();
		const RowQuickItemType& row = addedRowsIt.value();
		int index = row.first;
		const QMap<QString, QQuickItem*>& quickItems = row.second;
		
		if (_cachedRows.contains(name))
		{
			qDebug() << "A cached row had been found for " << name;
			QMap<QString, BoundQMLItem*>& cachedRow = _cachedRows[name].second;
			_resetQuickItems(quickItems, cachedRow);
			_rowsWithControls[name] = cachedRow;
			_cachedRows.remove(name);
		}
		else
			rowsNotFound[index] = qMakePair(name, quickItems);
	}
	
	// It there are some rows not found via the name of the variable,
	// Check if there is a cached row having the same index
	if (rowsNotFound.size() > 0 && _cachedRows.size() > 0)
	{
		QMap<int, QMap<QString, BoundQMLItem*> > remainingCachedRows;
		QMapIterator<QString, RowBoundItemType> cachedRowsIt(_cachedRows);
		while (cachedRowsIt.hasNext())
		{
			cachedRowsIt.next();
			const RowBoundItemType& cachedRow = cachedRowsIt.value();
			remainingCachedRows[cachedRow.first] = cachedRow.second;
		}
		
		QMapIterator<int, QMap<QString, BoundQMLItem*> > remainingCachedRowsIt(remainingCachedRows);
		while (remainingCachedRowsIt.hasNext())
		{
			remainingCachedRowsIt.next();
			int index = remainingCachedRowsIt.key();
			if (rowsNotFound.contains(index))
			{
				qDebug() << "Rows index " << index << " has apparently a new name: " << rowsNotFound[index].first;
				const QMap<QString, BoundQMLItem*>& cachedRow = remainingCachedRowsIt.value();
				_resetQuickItems(rowsNotFound[index].second, cachedRow);
				_rowsWithControls[rowsNotFound[index].first] = cachedRow;
				rowsNotFound.remove(index);
			}
		}
	}
	
	// If there are still some rows not found in the cache: build the BoundItem
	for (const QPair<QString, QMap<QString, QQuickItem*> >& quickItemRow : rowsNotFound.values())
	{	
		QString name = quickItemRow.first;
		QMap<QString, BoundQMLItem*> boundItemRow;
		for (QQuickItem* controlItem : quickItemRow.second.values())
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
				boundItemRow[boundQMLItem->name()] = boundQMLItem;
			}
		}
		qDebug() << "A new row has been added " << name;
		_rowsWithControls[name] = boundItemRow;
	}
}

void BoundQMLListViewDraggable::_resetQuickItems(const QMap<QString, QQuickItem*>& quickItems, const QMap<QString, BoundQMLItem*>& boundItems)
{
	QMapIterator<QString, QQuickItem*> quickItemsIt(quickItems);
	while (quickItemsIt.hasNext()) {
		quickItemsIt.next();
		BoundQMLItem* boundItem = boundItems[quickItemsIt.key()];
		if (!boundItem)
			qDebug() << "Cached Row does not have " << quickItemsIt.key();
		else
			boundItem->resetQMLItem(quickItemsIt.value());
	}
}
