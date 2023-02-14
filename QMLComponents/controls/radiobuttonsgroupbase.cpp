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
	_dependsOnDynamicComponents = true;
}

void RadioButtonsGroupBase::setUp()
{
	for (auto* button : _buttons)
	{
		if (button->property("checked").toBool())
			_setCheckedButton(button);
	}
	if (!checkedButton() && _buttons.size() > 0)
		_setCheckedButton(*(_buttons.begin()));

	JASPControl::setUp();
}

void RadioButtonsGroupBase::registerRadioButton(RadioButtonBase* button)
{
	const QString& controlName = button->name();
	if (form() && controlName.isEmpty())
		addControlError(tr("A RadioButton inside RadioButtonGroup element (name: %1) does not have any name").arg(name()));
	else
	{
		_buttons.insert(button);
		if(initialized())
		{
			// Case when Radio Button is dynamically added
			if (checkedButton() == nullptr || button->property("checked").toBool())
				_setCheckedButton(button);
		}
		emit buttonsChanged();
	}
}

void RadioButtonsGroupBase::unregisterRadioButton(RadioButtonBase* button)
{
	_buttons.remove(button);
	if (button == _selectedButton && _buttons.size() > 0)
		_setCheckedButton(*_buttons.begin());
	emit buttonsChanged();
}

void RadioButtonsGroupBase::radioButtonValueChanged(RadioButtonBase *button)
{
	if (_selectedButton == button)
	{
		emit valueChanged();
		setBoundValue(fq(value()));
	}
}


const QString RadioButtonsGroupBase::value() const
{
	return _selectedButton ? _selectedButton->name() : "";
}

void RadioButtonsGroupBase::bindTo(const Json::Value &jsonValue)
{
	BoundControlBase::bindTo(jsonValue);

	QString value = tq(jsonValue.asString());
	if (!value.isEmpty())
	{
		for (auto* button: qAsConst(_buttons))
		{
			if (button->name() == value)
			{
				_setCheckedButton(button);
				return;
			}
		}

		addControlError(tr("No radio button corresponding to name %1").arg(value));
		Log::log()  << "Known buttons: ";
		for (auto* button : _buttons) Log::log() << button->name() << ",";
		Log::log() << std::endl;
	}
}

Json::Value RadioButtonsGroupBase::createJson() const
{
	return fq(value());
}

bool RadioButtonsGroupBase::isJsonValid(const Json::Value &value) const
{
	return value.isString();
}

void RadioButtonsGroupBase::clickHandler(RadioButtonBase* button)
{
	if (!initialized()) return;

	if (button)
		_setCheckedButton(button);
	else
		Log::log() << "Object clicked is not a RadioButton item! Name" << button->objectName().toStdString();
	emit clicked();
}

void RadioButtonsGroupBase::_setCheckedButton(RadioButtonBase* button)
{
	QString buttonName = button->name();

	if (!_buttons.contains(button))
	{
		Log::log() << "Set Checked button of a radio button " << buttonName << " that is not registered" << std::endl;
		return;
	}

	if (_selectedButton != button)
	{
		_selectedButton = button; // Setting the checked property will call _setCheckedButton: so set first the _selectedButton so that it does set unnecessarily the button
		for (auto b : _buttons)
			b->setProperty("checked", _selectedButton == b);
		emit valueChanged();
		setBoundValue(fq(buttonName));
	}
}
