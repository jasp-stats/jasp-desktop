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

#include "listmodelmultinomialchi2test.h"

#include <iostream>
#include <fstream>

#include "../analysis/analysisform.h"
#include <QSize>
#include <QDebug>

#include "utilities/qutils.h"

using namespace std;

ListModelMultinomialChi2Test::ListModelMultinomialChi2Test(QMLListView* parent, QString tableType) :
	ListModel(parent)
{
	_tableType = tableType;
}


int ListModelMultinomialChi2Test::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	return _rowNames.length();
}

int ListModelMultinomialChi2Test::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	return int(_columnCount);
}

QVariant ListModelMultinomialChi2Test::data(const QModelIndex &index, int role) const
{
	if (_rowNames.length() == 0)
		return QVariant();

	int column = index.column();
	int row = index.row();

	if (column > -1 && column < columnCount() && row > -1 && row < _rowNames.length())
	{
		if(role == Qt::DisplayRole)
			return QVariant(_values[column][row]);
		else if(role == int(specialRoles::lines))
		{
			bool	belowMeIsActive = index.row() < rowCount() - 1;

			bool	up		= true,
					left	= true,
					down	= !belowMeIsActive,
					right	= index.column() == columnCount() - 1; //always draw left line and right line only if last col

			return	(left ?		1 : 0) +
					(right ?	2 : 0) +
					(up ?		4 : 0) +
					(down ?		8 : 0);
		}


	}

	return QVariant();
}


int ListModelMultinomialChi2Test::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	if (columnIndex >= _columnCount) return 0;

	return 6;
}

void ListModelMultinomialChi2Test::addColumn()
{
	beginResetModel();

	if (_columnCount < _maxColumn)
	{
		_colNames.push_back(_getColName(_columnCount));
		_columnCount++;
		QVector<double> newValues(_rowNames.length(), 1);
		_values.push_back(newValues);
	}

	endResetModel();

	emit modelChanged();
}

void ListModelMultinomialChi2Test::removeColumn(size_t col)
{
	beginResetModel();

	if (col < _columnCount)
	{
		_values.removeAt(int(col));
		_colNames.pop_back();
		_columnCount--;
	}

	endResetModel();

	emit modelChanged();
}

void ListModelMultinomialChi2Test::reset()
{
	beginResetModel();

	_colNames.clear();
	_values.clear();
	if (_rowNames.length() > 0)
	{
		_columnCount = 1;
		QVector<double> newValues(_rowNames.length(), 1);
		_values.push_back(newValues);
		_colNames.push_back(_getColName(0));
	}
	else
	{
		_columnCount = 0;
	}

	endResetModel();

	emit modelChanged();
}

void ListModelMultinomialChi2Test::itemChanged(int column, int row, double value)
{
	if (column > -1 && column < columnCount() && row > -1 && row < _rowNames.length())
	{
		if (_values[column][row] != value)
		{
			_values[column][row] = value;
			emit modelChanged();
		}
	}
}

void ListModelMultinomialChi2Test::initValues(const std::vector<std::string>& colNames, std::vector<std::string>& levels, const std::vector<std::vector<double> > &values)
{
	_columnCount = colNames.size();
	for (std::string colName : colNames)
		_colNames.push_back(QString::fromStdString(colName));
	for (std::string level : levels)
		_rowNames.push_back(QString::fromStdString(level));

	if (values.size() != _columnCount)
		addError("Wrong number of columns for Chi2 Test!!!");
	else if (values.size() > 0 && int(values[0].size()) != _rowNames.size())
		addError("Wrong number of rows for Chi2 Test!!!!");

	beginResetModel();

	for (size_t i = 0; i < values.size(); ++i)
	{
		QVector<double> colValues;
		for (double val : values[i])
			colValues.push_back(val);
		for (int j = int(values[i].size()); j < _rowNames.size(); j++)
			colValues.push_back(1);
		_values.push_back(colValues);
	}

	for (size_t i = values.size(); i < _columnCount; ++i)
	{
		QVector<double> extraColumn(_rowNames.length(), 1);
		_values.push_back(extraColumn);
	}

	endResetModel();
}


void ListModelMultinomialChi2Test::sourceTermsChanged(Terms *termsAdded, Terms *termsRemoved)
{
	Q_UNUSED(termsRemoved);

	beginResetModel();

	_rowNames.clear();
	_colNames.clear();
	_values.clear();
	_columnCount = 0;
	if (termsAdded && termsAdded->size() > 0)
	{
		const std::string& colName = termsAdded->at(0).asString();
		DataSet* dataset = listView()->form()->getDataSet();
		Column& column = dataset->columns().get(colName);
		Labels& labels = column.labels();
		for (auto it = labels.begin(); it != labels.end(); ++it)
		{
			_rowNames.push_back(tq(it->text()));
		}
		_columnCount = 1;
		QVector<double> newValues(_rowNames.length(), 1);
		_values.push_back(newValues);
		_colNames.push_back(_getColName(0));
	}

	endResetModel();

	emit modelChanged();
}

QString ListModelMultinomialChi2Test::_getColName(size_t index)
{
	if (_tableType == "PriorCounts")
		return tq("Counts");

	if (index >= _maxColumn)
		index = _maxColumn - 1;

	char letter = char(97 + index);
	return tq("H₀ (") + letter + tq(")");
}

QVariant ListModelMultinomialChi2Test::headerData( int section, Qt::Orientation orientation, int role) const
{
	if (role == Qt::DisplayRole && section >= 0)
	{
		if (orientation == Qt::Horizontal && section < _colNames.length())
		{
			return QVariant(_colNames[section]);
		}
		else if (section < _rowNames.length())
		{
			return QVariant(_rowNames[section]);
		}
	}
	else if (role == int(specialRoles::maxColString)) //A query from DataSetView for the maximumlength string to be expected! This to accomodate columnwidth
	{
		//calculate some maximum string?
		QString dummyText = headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX";
		int colWidth = getMaximumColumnWidthInCharacters(size_t(section));

		while(colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	else if(role == Qt::TextAlignmentRole)
		return QVariant(Qt::AlignCenter);

	return QVariant();
}

QHash<int, QByteArray> ListModelMultinomialChi2Test::roleNames() const
{
	QHash<int, QByteArray> roles = ListModel::roleNames();


	roles[int(specialRoles::active)]						= QString("active").toUtf8();
	roles[int(specialRoles::lines)]							= QString("lines").toUtf8();
	roles[int(specialRoles::maxColString)]					= QString("maxColString").toUtf8();

	return roles;
}

Qt::ItemFlags ListModelMultinomialChi2Test::flags(const QModelIndex &index) const
{
	Q_UNUSED(index);
	return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
}
