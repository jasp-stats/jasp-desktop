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

#include "qmlitem.h"
#include "../analysis/analysisform.h"
#include <QQmlProperty>
#include <QQuickItem>

QMLItem::QMLItem(QQuickItem *item, AnalysisForm* form)
	: _item(item), _form(form)
{
	_name = QQmlProperty(_item, "name").read().toString();
}

void QMLItem::cleanUp()
{
	if (_item)
		_item->disconnect();	
}

void QMLItem::resetQMLItem(QQuickItem *item)
{
	_item = item;
}

void QMLItem::addError(const QString &error)
{
	_form->addError(error);
}

bool QMLItem::addDependency(QMLItem *item)
{
	if (_depends.contains(item))
		return false;
	
	_depends.push_back(item);
	return true;
}

void QMLItem::setProperty(const QString& name, const QVariant& value)
{
	_item->setProperty(name.toStdString().c_str(), value);
}

QVariant QMLItem::getProperty(const QString &name)
{
	return _item->property(name.toStdString().c_str());
}

