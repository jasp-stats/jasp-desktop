//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "tablemodelvariablesoptions.h"

#include <boost/foreach.hpp>
#include <sstream>

#include <QSize>

#include "options/optionlist.h"
#include "options/optionterms.h"

#include "qutils.h"

using namespace std;

TableModelVariablesOptions::TableModelVariablesOptions(QObject *parent) :
	QAbstractTableModel(parent)
{
	_boundTo = NULL;
}

int TableModelVariablesOptions::rowCount(const QModelIndex &) const
{
	if (_boundTo == NULL)
		return 0;

	return _rows.size();
}

int TableModelVariablesOptions::columnCount(const QModelIndex &parent) const
{
	if (_boundTo == NULL)
		return 0;

	return _boundTo->rowTemplate()->size();
}

QVariant TableModelVariablesOptions::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	if (index.column() == 0)
	{
		if (role == Qt::DisplayRole)
		{	
			OptionTerms *option = static_cast<OptionTerms *>(_rows.at(index.row())->get(0));
			return Term(option->value().front()).asQString();
		}
		else
		{
			return QVariant();
		}
	}
	else
	{
		if (role != Qt::DisplayRole && role != Qt::EditRole)
			return QVariant();

		Option* option = _rows.at(index.row())->get(index.column());
		OptionList *list = dynamic_cast<OptionList *>(option);
		if (list == NULL)
			return QVariant("WTF");

		QString selected = tq(list->value());

		if (role == Qt::DisplayRole)
			return selected;

		QStringList items = tql(list->options());

		QList<QVariant> value;
		value.append(selected);
		value.append(items);

		return value;
	}

}

bool TableModelVariablesOptions::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo == NULL)
		return false;

	Options *row = _rows.at(index.row());
	Option  *cell = row->get(index.column());
	OptionList *list = dynamic_cast<OptionList *>(cell);

	if (list == NULL)
		return false;

	string oldValue = list->value();
	string newValue = fq(value.toString());

	if (oldValue != newValue)
	{
		list->setValue(newValue);
		emit dataChanged(index, index);
		_boundTo->setValue(_rows);
	}

	return true;
}

Qt::ItemFlags TableModelVariablesOptions::flags(const QModelIndex &index) const
{
	if (_boundTo == NULL)
		return 0;

	if (index.column() == 0)
		return Qt::ItemIsEnabled;

	return Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

QVariant TableModelVariablesOptions::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	if (role == Qt::DisplayRole && orientation == Qt::Horizontal)
	{
		string name;
		Option *option;
		_boundTo->rowTemplate()->get(section, name, option);
		return tq(name);
	}

	return QVariant();
}

void TableModelVariablesOptions::setVariables(const Terms &variables)
{
	if (_variables == variables)
		return;

	_variables = variables;

	if (_boundTo == NULL)
		return;

	beginResetModel();

	_rows.clear();

	BOOST_FOREACH(const Term &term, variables)
	{
		Options *row = static_cast<Options *>(_boundTo->rowTemplate()->clone());
		OptionTerms *termCell = static_cast<OptionTerms *>(row->get(0));
		termCell->setValue(term.scomponents());

		_rows.push_back(row);
	}

	endResetModel();

	_boundTo->setValue(_rows);
}

const Terms &TableModelVariablesOptions::variables() const
{
	return _variables;
}

void TableModelVariablesOptions::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_boundTo != NULL)
	{
		beginResetModel();
		_rows = _boundTo->value();
		endResetModel();
	}
}

void TableModelVariablesOptions::unbind()
{
	beginResetModel();

	_boundTo = NULL;
	_rows.clear();

	endResetModel();
}
