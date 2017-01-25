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

#include "itemmodelselectvariable.h"

#include "qutils.h"
#include "column.h"

using namespace std;

ItemModelSelectVariable::ItemModelSelectVariable(QObject *parent) :
	QAbstractListModel(parent)
{
	_source = NULL;
	_boundTo = NULL;
	_selectedIndex = 0;

	_nominalTextIcon = QIcon(":/icons/variable-nominal-text.svg");
	_nominalIcon = QIcon(":/icons/variable-nominal.svg");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
	_scaleIcon = QIcon(":/icons/variable-scale.svg");
}

int ItemModelSelectVariable::rowCount(const QModelIndex &parent) const
{
	if (_source == NULL)
		return 1;

	return _source->allVariables().size() + 1;
}

QVariant ItemModelSelectVariable::data(const QModelIndex &index, int role) const
{
	if (_source == NULL)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		if (index.row() == 0)
			return "[ None ]";
		else
			return _source->allVariables().at(index.row() - 1).asQString();
	}
	else if (role == Qt::CheckStateRole)
	{
		return index.row() == _selectedIndex;
	}
	else if (role == Qt::DecorationRole)
	{
		if (index.row() == 0)
			return QVariant();

		const Term &variable = _source->allVariables().at(index.row() - 1);
		int variableType = _source->requestInfo(variable, VariableInfo::VariableType).toInt();

		switch (variableType)
		{
		case Column::ColumnTypeNominalText:
			return QVariant(_nominalTextIcon);
		case Column::ColumnTypeNominal:
			return QVariant(_nominalIcon);
		case Column::ColumnTypeOrdinal:
			return QVariant(_ordinalIcon);
		case Column::ColumnTypeScale:
			return QVariant(_scaleIcon);
		default:
			return QVariant();
		}
	}
	else
	{
		return QVariant();
	}
}

Qt::ItemFlags ItemModelSelectVariable::flags(const QModelIndex &index) const
{
	if ( ! index.isValid())
		return Qt::ItemIsEnabled;

	return Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

bool ItemModelSelectVariable::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo != NULL && index.isValid() && role == Qt::CheckStateRole && value.canConvert(QVariant::Bool))
	{
		bool checked = value.toBool();

		if (checked)
		{
			_selectedIndex = index.row();
			if (_selectedIndex == 0)
				_boundTo->setValue("");
			else
				_boundTo->setValue(_source->allVariables().at(_selectedIndex - 1).asString());
		}

		return true;
	}
	else
	{
		return false;
	}
}

void ItemModelSelectVariable::bindTo(Option *option)
{
	beginResetModel();

	_boundTo = dynamic_cast<OptionVariable *>(option);
	updateSelected();

	endResetModel();
}

void ItemModelSelectVariable::setSource(TableModelVariablesAvailable *source)
{
	beginResetModel();
	_source = source;
	updateSelected();
	connect(_source, SIGNAL(variablesChanged()), this, SLOT(variablesChangedHandler()));
	endResetModel();
}

void ItemModelSelectVariable::variablesChangedHandler()
{
	beginResetModel();
	updateSelected();
	endResetModel();
}

void ItemModelSelectVariable::updateSelected()
{
	/*if (_source == NULL || _boundTo == NULL)
		return;

	_selectedIndex = 0;

	for (int i = 0; i < _source->allVariables().size(); i++)
	{
		if (fq(_source->allVariables().at(i).first) == _boundTo->value()[0])
		{
			_selectedIndex = i + 1;
			break;
		}
	}*/

}


