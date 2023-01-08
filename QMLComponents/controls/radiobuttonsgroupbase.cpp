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
}

void RadioButtonsGroupBase::registerRadioButton(RadioButtonBase* button)
{
	const QString& controlName = button->name();
	if (controlName.isEmpty())
		addControlError(tr("A RadioButton inside RadioButtonGroup element (name: %1) does not have any name").arg(name()));
	else
	{
		if (_valueButtonMap.size() == 0)
			_setCheckedButton(button);
		if (button->property("checked").toBool())
			_setCheckedButton(button);
		_valueButtonMap[controlName] = button;
		_buttonValueMap[button] = controlName;
		emit buttonsChanged();
	}
}

void RadioButtonsGroupBase::unregisterRadioButton(RadioButtonBase* button)
{
	_valueButtonMap.remove(button->name());
	_buttonValueMap.remove(button);
	if (_value == button->name() && _valueButtonMap.size() > 0)
		_setCheckedButton(_valueButtonMap.first());
	emit buttonsChanged();
}


void RadioButtonsGroupBase::radioButtonValueChanged(RadioButtonBase *button)
{
	if (_buttonValueMap.contains(button))
	{
		QString oldValue = _buttonValueMap[button];
		_valueButtonMap.remove(oldValue);
		_buttonValueMap.remove(button);
		_valueButtonMap.insert(button->name(), button);
		_buttonValueMap.insert(button, button->name());
		if (_value == oldValue)
			_setCheckedButton(button);
		emit buttonsChanged();
	}
}

void RadioButtonsGroupBase::bindTo(const Json::Value &jsonValue)
{
	BoundControlBase::bindTo(jsonValue);

	QString value = tq(jsonValue.asString());
	if (!value.isEmpty())
	{
		if (!_valueButtonMap.contains(value))
		{
			addControlError(tr("No radio button corresponding to name %1").arg(value));
			QStringList names = _valueButtonMap.keys();
			Log::log()  << "Known button: " << names.join(',').toStdString() << std::endl;
		}
		else
			_setCheckedButton(_valueButtonMap[value]);
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
	if (button)
		_setCheckedButton(button);
	else
		Log::log() << "Object clicked is not a RadioButton item! Name" << button->objectName().toStdString();
	emit clicked();
}

void RadioButtonsGroupBase::_setCheckedButton(RadioButtonBase* button)
{
	QString buttonName = button->name();
	if (_value != buttonName)
	{
		if (_valueButtonMap.contains(_value))
			_valueButtonMap[_value]->setProperty("checked", false);
		button->setProperty("checked", true);
		setValue(buttonName);
		setBoundValue(fq(buttonName));
	}
}
