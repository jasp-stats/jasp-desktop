//
// Copyright (C) 2013-2020 University of Amsterdam
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

#include "radiobuttonsgroupbase.h"
#include "radiobuttonbase.h"
#include <QQmlProperty>
#include <QQuickItem>
#include "log.h"


using namespace std;

RadioButtonsGroupBase::RadioButtonsGroupBase(QQuickItem* item)
	: JASPControl(item), BoundControlBase(this)
{
	_controlType = ControlType::RadioButtonGroup;
}

void RadioButtonsGroupBase::setUp()
{
	JASPControl::setUp();
	QList<RadioButtonBase* > buttons;
	_getRadioButtons(this, buttons);
	QVariant buttonGroup = property("buttonGroup");

	connect(this,	&RadioButtonsGroupBase::clicked, this,	&RadioButtonsGroupBase::clickedSlot);

	for (RadioButtonBase* button: buttons)
	{
		const QString& controlName = button->name();
		if (controlName.isEmpty())
			addControlError(tr("A RadioButton inside RadioButtonGroup element (name: %1) does not have any name").arg(name()));
		else
		{
			_buttons[controlName] = button;
			if (button->property("checked").toBool())	setValue(controlName);
			button->setProperty("buttonGroup", buttonGroup);
		}
	}

	if (value().isEmpty() && _buttons.size() > 0)
	{
		RadioButtonBase* firstButton = _buttons.values()[0];
		Log::log() << "No checked button found in radio buttons " << name() << ". First one (" << firstButton->name() << ") is checked per default" << std::endl;
		_setCheckedButton(firstButton);
	}
}

void RadioButtonsGroupBase::_getRadioButtons(QQuickItem* item, QList<RadioButtonBase *> &buttons) {
	for (QQuickItem* child : item->childItems())
	{
		JASPControl* jaspControl = qobject_cast<JASPControl*>(child);
		if (jaspControl)
		{
			ControlType controlType = jaspControl->controlType();
			if (controlType == ControlType::RadioButton)
				buttons.append(qobject_cast<RadioButtonBase*>(jaspControl));
			else if (controlType != ControlType::RadioButtonGroup)
				_getRadioButtons(child, buttons);
		}
		else
			_getRadioButtons(child, buttons);
	}	
}

void RadioButtonsGroupBase::bindTo(const Json::Value &jsonValue)
{
	BoundControlBase::bindTo(jsonValue);

	string value = jsonValue.asString();
	if (!value.empty())
	{
		RadioButtonBase* button = _buttons[tq(value)];
		if (!button)
		{
			addControlError(tr("No radio button corresponding to name %1").arg(QString::fromStdString(value)));
			QStringList names = _buttons.keys();
			Log::log()  << "Known button: " << names.join(',').toStdString() << std::endl;
		}
		else
			_setCheckedButton(button);
	}
}

Json::Value RadioButtonsGroupBase::createJson()
{
	return fq(value());
}

bool RadioButtonsGroupBase::isJsonValid(const Json::Value &value)
{
	return value.isString();
}

void RadioButtonsGroupBase::clickedSlot(const QVariant& button)
{
	QObject* objButton = button.value<QObject*>();
	if (objButton)
		objButton = objButton->parent();
	RadioButtonBase *radioButton = qobject_cast<RadioButtonBase*>(objButton);
	if (radioButton)
		_setCheckedButton(radioButton);
	else
		Log::log() << "Object clicked is not a RadioButton item! Name" << objButton->objectName().toStdString();
}

void RadioButtonsGroupBase::_setCheckedButton(RadioButtonBase* button)
{
	QString buttonName = button->name();
	button->setProperty("checked", true);

	// Ensure that other buttons are unchecked
	for (RadioButtonBase* otherButton : _buttons)
		if (otherButton != button)
			otherButton->setProperty("checked", false);

	if (_value != buttonName)
	{
		setValue(buttonName);
		setBoundValue(fq(buttonName));
	}
}
