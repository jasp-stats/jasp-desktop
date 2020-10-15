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

#include "boundqmlcheckbox.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrolbase.h"
#include "log.h"

BoundQMLCheckBox::BoundQMLCheckBox(JASPControlBase* item)
	: JASPControlWrapper(item)
	, QObject(item)
	, BoundQMLItem()
{
	if (item)
		QQuickItem::connect(item, SIGNAL(clicked()), this, SLOT(checkBoxClickedSlot()));
}

void BoundQMLCheckBox::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionBoolean *>(option);

	if (_boundTo != nullptr)
	{
		_checked = _boundTo->value();
		setItemProperty("checked", _checked);
	}
	else
		Log::log()  << "could not bind to OptionBoolean in BoundQuickCheckBox.cpp" << std::endl;
}

bool BoundQMLCheckBox::isOptionValid(Option* option)
{
	return dynamic_cast<OptionBoolean*>(option) != nullptr;
}

bool BoundQMLCheckBox::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::booleanValue;
}

Option *BoundQMLCheckBox::createOption()
{
	QVariant checkedVariant = getItemProperty("checked");
	if (!checkedVariant.isNull())
		_checked = checkedVariant.toBool();
	return new OptionBoolean(_checked);
}

void BoundQMLCheckBox::resetQMLItem(JASPControlBase *item)
{
	BoundQMLItem::resetQMLItem(item);
	setItemProperty("checked", _checked);
	if (_item)
		QQuickItem::connect(_item, SIGNAL(clicked()), this, SLOT(checkBoxClickedSlot()));
}

void BoundQMLCheckBox::setQMLItemChecked(bool checked)
{
	_checked = checked;
	setItemProperty("checked", checked);
}

void BoundQMLCheckBox::checkBoxClickedSlot()
{
	_checked = getItemProperty("checked").toBool();
	if (_boundTo != nullptr)
		_boundTo->setValue(_checked);
}
