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

#include "radiobuttonbase.h"
#include "radiobuttonsgroupbase.h"

RadioButtonBase::RadioButtonBase(QQuickItem* item)
	: JASPControl(item)
{
	_controlType		= ControlType::RadioButton;
	_isBound			= false;
	_nameIsOptionValue	= true;

	connect(this, &QQuickItem::parentChanged, this, &RadioButtonBase::registerWithParent);
	connect(this, &JASPControl::nameChanged, this, &RadioButtonBase::valueChangeHandler);
}

void RadioButtonBase::registerWithParent()
{
	// Warning: this slot can be called either by the Component.onCompleted of the RadioButton.qml or by a parentChanged signal

	QQuickItem* ancestor = parentItem();
	while(ancestor)
	{
		RadioButtonsGroupBase* group = qobject_cast<RadioButtonsGroupBase*>(ancestor);
		if(group)
		{
			if (_group)
			{
				if (_group == group) return; // Already registered

				//We are already registered somewhere so lets undo that
				_group->unregisterRadioButton(this);
			}
			_group = group;
			_group->registerRadioButton(this);
			break;
		}
		ancestor = ancestor->parentItem();
	}
}

void RadioButtonBase::clickHandler()
{
	if (_group)
		_group->clickHandler(this);
}

void RadioButtonBase::valueChangeHandler()
{
	if (_group)
		_group->radioButtonValueChanged(this);
}
