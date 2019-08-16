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
#include "../analysis/analysisform.h"
#include "listmodelassignedinterface.h"
#include "qmllistviewtermsavailable.h"
#include "analysis/options/optionstable.h"
#include "analysis/options/optionvariable.h"
#include "analysis/options/optionboolean.h"
#include "analysis/options/optionlist.h"
#include "analysis/options/optionstring.h"

#include <QQuickItem>
#include <QQmlProperty>

#include <QTimer>

BoundQMLListViewDraggable::BoundQMLListViewDraggable(QQuickItem *item, AnalysisForm *form)
	: QMLListViewDraggable(item, form)
	, BoundQMLItem()
{
	QStringList extraControlTitles;
	
	QList<QVariant> extraControlColumnsVariants = QQmlProperty(_item, "extraControlColumns").read().toList();
	for (const QVariant& extraControlColumnVariant : extraControlColumnsVariants)
	{
		QMap<QString, QVariant> properties;
		
		QObject* extraControlColumnObject = extraControlColumnVariant.value<QObject*>();
		const QMetaObject *meta = extraControlColumnObject->metaObject();
		int propertyCount = meta->propertyCount();
		for (int i = 0; i < propertyCount; ++i)
		{
			QMetaProperty property = meta->property(i);
			QString key = QString::fromLatin1(property.name());
			QVariant value = property.read(extraControlColumnObject);
			if (key == "purpose")
			{
				if (value.toString() == "nuisance")
					_hasNuisanceControl = true;
			}
			else if (key == "title")
			{
				QString title = value.toString();
				if (!title.isEmpty())
					extraControlTitles.push_back(value.toString());
			}
			else if (key != "properties")
			{
				if (key == "type" && value.toString() == "DropDown")
					// DropDown is an alias to ComboBox
					value = "ComboBox";
				properties[key] = value;
			}
			else
			{
				QMap<QString, QVariant> map = value.toMap();
				QMapIterator<QString, QVariant> it(map);
				while (it.hasNext())
				{
					it.next();
					properties[it.key()] = it.value();
				}

			}
		}

		if (_hasNuisanceControl)
			_optionNuisanceName = properties["name"].toString().toStdString();
		_extraControlColumns.push_back(properties);
	}
	
	if (_extraControlColumns.length() > 0)
	{
		_item->setProperty("extraControlTitles", extraControlTitles);
		_hasExtraControls = true;
	}
}

void BoundQMLListViewDraggable::setUp()
{	
	QMLListViewDraggable::setUp();
	
	ListModel* availableModel = _form->getRelatedModel(this);
	ListModelAssignedInterface* _assignedModel = assignedModel();
	
	if (!availableModel)
	{
		if (sourceModels().empty() && !_item->property("debug").toBool())
			addError(QString::fromLatin1("Cannot find source for VariableList ") + name());
	}
	else
	{
		_availableModel = dynamic_cast<ListModelAvailableInterface*>(availableModel);
		if (!_availableModel)
			addError(QString::fromLatin1("Wrong kind of source for VariableList ") + name());
		else
		{
			_assignedModel->setAvailableModel(_availableModel);
			QMLListViewTermsAvailable* qmlAvailableListView = dynamic_cast<QMLListViewTermsAvailable*>(_availableModel->listView());
			if (qmlAvailableListView)
			{
				qmlAvailableListView->addAssignedModel(_assignedModel);
				addDependency(qmlAvailableListView);
			}
			connect(_availableModel, &ListModelAvailableInterface::allAvailableTermsChanged, _assignedModel, &ListModelAssignedInterface::availableTermsChanged);
		}
	}
	
	_assignedModel->addExtraControls(_extraControlColumns);
	
}

ListModelAssignedInterface* BoundQMLListViewDraggable::assignedModel()
{
	return dynamic_cast<ListModelAssignedInterface*>(model());
}
