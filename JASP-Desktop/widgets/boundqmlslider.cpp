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

#include "boundqmlslider.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrolbase.h"
#include <QQmlProperty>
#include <QQuickItem>
#include <QAbstractListModel>
#include <QTimer>
#include "log.h"

BoundQMLSlider::BoundQMLSlider(JASPControlBase* item)
	: JASPControlWrapper(item)
	, QObject(item)
	, BoundQMLItem()
{
	QQuickItem::connect(item, SIGNAL(moved()), this, SLOT(sliderMovedSlot()));
}

void BoundQMLSlider::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionNumber *>(option);
	if (_boundTo != nullptr)
	{
		_number = _boundTo->value();
		setItemProperty("value", _number);
	}
	else
		Log::log()  << "Option is not an OptionNumber in BoundQMLSlider" << std::endl;
}


void BoundQMLSlider::resetQMLItem(JASPControlBase *item)
{
	BoundQMLItem::resetQMLItem(item);
	setItemProperty("value", _number);
	
	if (_item)
	{
		QQuickItem::connect(_item, SIGNAL(moved()), this, SLOT(sliderMovedSlot()));
		QQuickItem::connect(_item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
	}
}

Option *BoundQMLSlider::createOption()
{
	_number = _item->property("value").toDouble();	
	OptionNumber* option = new OptionNumber();
	option->setValue(_number);
	
	return option;
}

bool BoundQMLSlider::isOptionValid(Option *option)
{
	return dynamic_cast<OptionNumber*>(option) != nullptr;
}

bool BoundQMLSlider::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::realValue || optionValue.type() == Json::intValue;
}

void BoundQMLSlider::sliderMovedSlot()
{
	double newValue = QQmlProperty(_item, "value").read().toDouble();
	
	if (newValue == _number)
		return;
	
	_number = newValue;
	
	if (_boundTo != nullptr)
	{
		if (_changing)
			return;
		_changing = true;
		QTimer::singleShot(300, this, SLOT(_changeOptionHandler()));
	}
}

void BoundQMLSlider::_changeOptionHandler()
{
	Log::log()  << "Slider set new value: " << _number << std::endl;
	_boundTo->setValue(_number);
	_changing = false;
}
