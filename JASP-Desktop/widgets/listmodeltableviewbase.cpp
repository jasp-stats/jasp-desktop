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

#include "log.h"
#include <QSize>
#include <fstream>
#include "listmodeltableviewbase.h"
#include "../analysis/analysisform.h"
#include "utilities/qutils.h"
#include "boundqmltableview.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optiondoublearray.h"

using namespace std;

ListModelTableViewBase::ListModelTableViewBase(BoundQMLTableView * tableView, QString tableType)
	: ListModel(tableView), _tableView(tableView), _tableType(tableType)
{
	connect(this, &ListModel::modelChanged, this, &ListModelTableViewBase::modelChangedSlot);
}

QVariant ListModelTableViewBase::data(const QModelIndex &index, int role) const
{
	if (_rowNames.length() == 0)
		return QVariant();

	int		column	= index.column(),
			row		= index.row();

	if (column < 0 || column >= columnCount() || row < 0 || row >= _rowNames.length())
		return QVariant();

	switch(role)
	{
	case int(specialRoles::lines):
	{
		bool	belowMeIsActive = row < rowCount() - 1,
				up				= true,
				left			= true,
				down			= !belowMeIsActive,
				right			= column == columnCount() - 1; //always draw left line and right line only if last col

		return	(left ?		1 : 0) +
				(right ?	2 : 0) +
				(up ?		4 : 0) +
				(down ?		8 : 0);
	}
	case Qt::DisplayRole:	return QVariant(_values[column][row]);
	default:				return QVariant();
	}
}


int ListModelTableViewBase::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	int maxL = 3;
	for(double val : _values[columnIndex])
		maxL = std::max(QVariant(val).toString().size(), maxL);

	return maxL + 3;
}

void ListModelTableViewBase::addColumn()
{
	beginResetModel();

	if (columnCount() < _maxColumn)
	{
		_colNames.push_back(getColName(columnCount()));
		_values.push_back(QVector<double>(_rowNames.length(), 1));
		_columnCount++;
	}

	endResetModel();

	emit columnCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::removeColumn(size_t col)
{
	beginResetModel();

	if (col < columnCount())
	{
		_values.removeAt(int(col));
		_colNames.pop_back();	
		_columnCount--;
	}

	endResetModel();

	emit columnCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::reset()
{
	beginResetModel();

	_colNames.clear();
	_values.clear();

	if (_rowNames.length() > 0)
	{
		QVector<double> newValues(_rowNames.length(), 1);
		_values.push_back(newValues);
		_colNames.push_back(getColName(0));
		_columnCount = 1;
	}
	else
		_columnCount = 0;

	endResetModel();

	emit columnCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::itemChanged(int column, int row, double value)
{
	//If you change this function, also take a look at ListModelFilteredDataEntry::itemChanged
	if (column > -1 && column < columnCount() && row > -1 && row < _rowNames.length())
	{
		if (_values[column][row] != value)
		{
			bool gotLarger = QVariant(_values[column][row]).toString().size() != QVariant(value).toString().size();
			_values[column][row] = value;

			emit dataChanged(index(row, column), index(row, column), { Qt::DisplayRole });
			emit modelChanged();

			if(gotLarger)
				emit headerDataChanged(Qt::Orientation::Horizontal, column, column);
		}
	}
}

QVariant ListModelTableViewBase::headerData( int section, Qt::Orientation orientation, int role) const
{
	if(section < 0 || section >= (orientation == Qt::Horizontal ? _colNames.length() : _rowNames.length()))
		return QVariant();

	switch(role)
	{
	case int(specialRoles::maxColString): //A query from DataSetView for the maximumlength string to be expected! This to accomodate columnwidth
	{
		QString dummyText	= headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX";
		int colWidth		= getMaximumColumnWidthInCharacters(size_t(section));

		while(colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	case Qt::DisplayRole:			return QVariant(orientation == Qt::Horizontal ? _colNames[section] : _rowNames[section]);
	case Qt::TextAlignmentRole:		return QVariant(Qt::AlignCenter);
	default:						return QVariant();
	}
}

QHash<int, QByteArray> ListModelTableViewBase::roleNames() const
{
	static QHash<int, QByteArray> roles = ListModel::roleNames();

	static bool addRoles = true;

	if(addRoles)
	{
		roles[int(specialRoles::active)]		= QString("active").toUtf8();
		roles[int(specialRoles::lines)]			= QString("lines").toUtf8();
		roles[int(specialRoles::maxColString)]	= QString("maxColString").toUtf8();
		addRoles = false;
	}

	return roles;
}

Qt::ItemFlags ListModelTableViewBase::flags(const QModelIndex &) const
{
	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

void ListModelTableViewBase::runRScript(const QString & script)
{
	_tableView->runRScript(script);
}
