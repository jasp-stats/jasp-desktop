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
#include "analysisform.h"
#include <limits>
#include <QTimer>
#include "log.h"

SliderBase::SliderBase(QQuickItem* parent)
	: JASPControl(parent), BoundControlBase(this)
{
	_controlType = ControlType::Slider;
}

bool SliderBase::isJsonValid(const Json::Value &value) const
{
	return value.isNumeric();
}

Json::Value SliderBase::createJson() const
{
	return property("value").toDouble();
}

void SliderBase::bindTo(const Json::Value &value)
{
	BoundControlBase::bindTo(value);
	setProperty("value", value.asDouble());
}

void SliderBase::setUp()
{
	JASPControl::setUp();
	connect(this,	&SliderBase::moved, this,	&SliderBase::movedSlot);
}

void SliderBase::movedSlot()
{
	if (_changing)
		return;
	_changing = true;
	QTimer::singleShot(300, this, SLOT(_movedDelayedSlot()));
}

void SliderBase::_movedDelayedSlot()
{
	setBoundValue(property("value").toDouble());
	_changing = false;
}
