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

#include "listmodel.h"
#include "analysis/analysisqmlform.h"

#include <QQmlProperty>

ListModel::ListModel(AnalysisQMLForm *form, QQuickItem *item) 
	: QAbstractListModel(form)
	, _form(form)
	, _item(item)
{
	// Cannot set the model to item during the constructor: it crashes.
	// It is done during the setUp.
	_isSetUp = false;
	_areTermsVariables = true;
	_name = QQmlProperty(_item, "name").read().toString();
}

QHash<int, QByteArray> ListModel::roleNames() const
{
	QHash<int, QByteArray> roles;
	roles[TypeRole] = "type";
	roles[NameRole] = "name";
	return roles;
}

void ListModel::setUp()
{
	if (!_isSetUp)
		QQmlProperty(_item, "model").write(QVariant::fromValue(this));					
	_isSetUp = true;
}

void ListModel::refresh() {
	beginResetModel(); 
	endResetModel();
}

void ListModel::addError(const QString &error) const
{
	_form->addError(error);
}

void ListModel::setTermsAreNotVariables()
{
	_areTermsVariables = false;
	QQmlProperty::write(_item, "showVariableIcon", false);
}
