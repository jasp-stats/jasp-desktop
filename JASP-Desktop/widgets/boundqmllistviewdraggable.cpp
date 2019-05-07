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
			else
				properties[key] = value;
		}

		if (_hasNuisanceControl)
			_nuisanceName = properties["name"].toString().toStdString();
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
			connect(_availableModel, &ListModelAvailableInterface::modelChanged, _assignedModel, &ListModelAssignedInterface::availableTermsChanged);			
		}
	}
	
	_assignedModel->addExtraControls(_extraControlColumns);
	
}

ListModelAssignedInterface* BoundQMLListViewDraggable::assignedModel()
{
	return dynamic_cast<ListModelAssignedInterface*>(model());
}

void BoundQMLListViewDraggable::addExtraOptions(Options *options)
{
	for (const QMap<QString, QVariant>& properties : _extraControlColumns)
	{
		QString type = properties["type"].toString();
		Option* option = nullptr;
		if (type == "CheckBox")
		{
			bool checked = (properties.contains("checked") ? properties["checked"].toBool() : false);
			option = new OptionBoolean(checked);
		}
		else if (type == "ComboBox")
		{
			QString defaultValue = (properties.contains("value") ? properties["value"].toString() : "");
			QStringList valueList = (properties.contains("values") ? properties["values"].toStringList() : QStringList());
			std::vector<std::string> values;
			for (const QString& oneValue : valueList)
				values.push_back(oneValue.toStdString());
			option = new OptionList(values, defaultValue.toStdString());
		}
		else if (type == "TextField")
		{
			QString value = (properties.contains("value") ? properties["value"].toString() : "");
			option = new OptionString(value.toStdString());
		}
		if (option)
			options->add(properties["name"].toString().toStdString(), option);
		else
			addError(QString::fromLatin1("Extra column in ") + name() + " has an unsupported type: " + type);
	}
	
}
