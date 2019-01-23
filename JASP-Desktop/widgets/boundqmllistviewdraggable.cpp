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
#include <QDebug>
#include <QTimer>

BoundQMLListViewDraggable::BoundQMLListViewDraggable(QQuickItem *item, AnalysisForm *form)
	: QMLListViewDraggable(item, form)
	, BoundQMLItem(item, form) 
{
	_extraControlVariableName = QQmlProperty(_item, "extraControlVariableName").read().toString().toStdString();
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
			bool addValue = true;
			switch (QMetaType::Type(property.type()))
			{
			case QMetaType::Int:
				if (value.toInt() == 0) addValue = false;
				break;
			case QMetaType::QString:
				if (value.toString().isEmpty()) addValue = false;
				if (key == "title")
				{
					extraControlTitles.push_back(value.toString());
					addValue = false;
				}
				break;
			default:
				addValue = true;
			}
			if (addValue)
				properties[key] = value;
		}

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
	
	ListModel* sourceModel = _form->getRelatedModel(this);
	ListModelAssignedInterface* _assignedModel = assignedModel();
	
	if (!sourceModel)
	{
		if (syncModels().empty())
			addError(QString::fromLatin1("Cannot find source ListView for item ") + name());
	}
	else
	{
		_sourceModel = dynamic_cast<ListModelAvailableInterface*>(sourceModel);
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
			option = new OptionBoolean();
		else if (type == "ComboBox")
			option = new OptionList(std::vector<std::string>());
		else if (type == "TextField")
			option = new OptionString();
		if (option)
			options->add(properties["name"].toString().toStdString(), option);
		else
			addError(QString::fromLatin1("Extra column in ") + name() + " has an unsupported type: " + type);
	}
	
}
