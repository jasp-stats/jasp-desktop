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

#include "sliderbase.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrol.h"
#include <QQmlProperty>
#include <QQuickItem>
#include <QAbstractListModel>
#include <QTimer>
#include "log.h"

SliderBase::SliderBase(QQuickItem* parent)
	: JASPControl(parent)
{
	_controlType = ControlType::Slider;
	QQuickItem::connect(this, SIGNAL(moved()), this, SLOT(sliderMovedSlot()));
}

void SliderBase::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionNumber *>(option);
	if (_boundTo != nullptr)
	{
		_number = _boundTo->value();
		setProperty("value", _number);
	}
	else
		Log::log()  << "Option is not an OptionNumber in BoundQMLSlider" << std::endl;
}

Option *SliderBase::createOption()
{
	_number = property("value").toDouble();
	OptionNumber* option = new OptionNumber();
	option->setValue(_number);
	
	return option;
}

bool SliderBase::isOptionValid(Option *option)
{
	return dynamic_cast<OptionNumber*>(option) != nullptr;
}

bool SliderBase::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::realValue || optionValue.type() == Json::intValue;
}

void SliderBase::sliderMovedSlot()
{
	double newValue = property("value").toDouble();
	
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

void SliderBase::_changeOptionHandler()
{
	Log::log()  << "Slider set new value: " << _number << std::endl;
	_boundTo->setValue(_number);
	_changing = false;
}
