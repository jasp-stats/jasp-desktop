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

#include "boundqmlitem.h"
#include "analysisqmlform.h"
#include <QQmlProperty>
#include <QQuickItem>


using namespace std;

BoundQMLItem::BoundQMLItem(QQuickItem *item, AnalysisQMLForm* form)
	: QObject(form), _item(item), _form(form)
{
	_name = QQmlProperty(_item, "name").read().toString();
}

BoundQMLItem::~BoundQMLItem()
{
	
}

const QString& BoundQMLItem::name()
{
	return _name;
}

void BoundQMLItem::setUp()
{
}

void BoundQMLItem::resetQMLItem(QQuickItem *item)
{
	_item = item;
}

void BoundQMLItem::addError(const QString &error)
{
	_form->addError(error);
}
