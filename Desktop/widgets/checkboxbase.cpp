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

#include "checkboxbase.h"
#include "../analysis/analysisform.h"
#include "log.h"

CheckBoxBase::CheckBoxBase(QQuickItem* parent)
	: JASPControl(parent)
{
	_controlType = ControlType::CheckBox;
}

void CheckBoxBase::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionBoolean *>(option);

	if (_boundTo != nullptr)
	{
		_checked = _boundTo->value();
		setProperty("checked", _checked);
	}
	else
		Log::log()  << "could not bind to OptionBoolean in BoundQuickCheckBox.cpp" << std::endl;
}

bool CheckBoxBase::isOptionValid(Option* option)
{
	return dynamic_cast<OptionBoolean*>(option) != nullptr;
}

bool CheckBoxBase::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::booleanValue;
}

void CheckBoxBase::setUp()
{
	JASPControl::setUp();
	QQuickItem::connect(this, SIGNAL(clicked()), this, SLOT(checkBoxClickedSlot()));
}

Option *CheckBoxBase::createOption()
{
	QVariant checkedVariant = property("checked");
	if (!checkedVariant.isNull())
		_checked = checkedVariant.toBool();
	return new OptionBoolean(_checked);
}

void CheckBoxBase::setQMLItemChecked(bool checked)
{
	_checked = checked;
	setProperty("checked", checked);
}

void CheckBoxBase::checkBoxClickedSlot()
{
	_checked = property("checked").toBool();
	if (_boundTo != nullptr)
		_boundTo->setValue(_checked);
}
