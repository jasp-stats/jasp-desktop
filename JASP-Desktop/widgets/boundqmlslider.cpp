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
#include "analysis/analysisqmlform.h"
#include <QQmlProperty>
#include <QQuickItem>
#include <QAbstractListModel>
#include <QTimer>

BoundQMLSlider::BoundQMLSlider(QQuickItem* item, AnalysisQMLForm* form) 
	: QMLItem(item, form)
	, QObject(form)
	, BoundQMLItem(item, form)
{
	_boundTo = NULL;
	_number = 0;
	QQuickItem::connect(item, SIGNAL(moved()), this, SLOT(sliderMovedSlot()));
}

void BoundQMLSlider::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionNumber *>(option);
	_number = _boundTo->value();
	_item->setProperty("value", _number);
}


void BoundQMLSlider::resetQMLItem(QQuickItem *item)
{
	BoundQMLItem::resetQMLItem(item);
	
	_item->setProperty("value", _number);
	QQuickItem::connect(_item, SIGNAL(moved()), this, SLOT(sliderMovedSlot()));
	QQuickItem::connect(_item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
}

Option *BoundQMLSlider::createOption()
{
	_number = _item->property("value").toFloat();	
	OptionNumber* option = new OptionNumber();
	option->setValue(_number);
	
	return option;
}

bool BoundQMLSlider::_changing = false;

void BoundQMLSlider::sliderMovedSlot()
{
	float newValue = QQmlProperty(_item, "value").read().toFloat();
	
	if (newValue == _number)
		return;
	
	_number = newValue;
	
	if (_boundTo != NULL)
	{
		if (_changing)
			return;
		_changing = true;
		QTimer::singleShot(300, this, SLOT(_changeOptionHandler()));
	}
}

void BoundQMLSlider::_changeOptionHandler()
{
	qDebug() << "Slider set new value: " << _number;
	_boundTo->setValue(_number);
	_changing = false;
}
