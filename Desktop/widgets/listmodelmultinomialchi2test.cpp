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
#include "utilities/qutils.h"
#include "listmodelmultinomialchi2test.h"
#include "analysis/analysisform.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optiondoublearray.h"

ListModelMultinomialChi2Test::ListModelMultinomialChi2Test(TableViewBase * parent, QString tableType)
	: ListModelTableViewBase(parent, tableType)
{
	_defaultCellVal		= 1;
	_initialColCnt		= 1;
	_keepRowsOnReset	= true;
}

void ListModelMultinomialChi2Test::sourceTermsReset()
{
	beginResetModel();

	_rowNames.clear();
	_colNames.clear();
	_values.clear();
	_columnCount = 0;
	_rowCount    = 0;

	Terms newTerms = getSourceTerms();
	if (newTerms.size() > 0)
	{
		_columnBeingTracked	= tq(newTerms.at(0).asString());
		_rowNames			= requestInfo(_columnBeingTracked, VariableInfo::Labels).toStringList();
		_rowCount			= size_t(_rowNames.size());

		QVector<QVariant> newValues(_rowNames.length(), 1.0);
		_values.push_back(newValues);
		_colNames.push_back(getDefaultColName(0));
		_columnCount = 1;

	}

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

int ListModelMultinomialChi2Test::sourceLabelChanged(QString columnName, QString originalLabel, QString newLabel)
{
	if(columnName != _columnBeingTracked)
		return -1;

	for(int row=0; row<_rowNames.size(); row++)
		if(_rowNames[row] == originalLabel)
		{
			_rowNames[row] = newLabel;
			QModelIndex i = index(row, 0);
			emit dataChanged(i, i);
			break;
		}

	return 0;
}

int ListModelMultinomialChi2Test::sourceLabelsReordered(QString columnName)
{
	if(columnName != _columnBeingTracked)
		return -1;

	std::map<QString, std::vector<QVariant>> tempStore;

	//because everything is stored in columns we first need to map all the rows, to well rows (with the name being key)
	for(int row=0; row<_rowCount; row++)
		for(int col=0; col<_columnCount; col++)
			tempStore[_rowNames[row]].push_back(_values[col][row]);

	beginResetModel();
	_rowNames	= requestInfo(_columnBeingTracked, VariableInfo::Labels).toStringList();
	_values.clear();
	_values.resize(_columnCount);

	for(int row=0; row<_rowCount; row++)
		for(int col=0; col<_columnCount; col++)
			_values[col].push_back(tempStore[_rowNames[row]][col]);

	endResetModel();

	return 0;
}


QString ListModelMultinomialChi2Test::getDefaultColName(size_t index) const
{
	if (_tableType == "PriorCounts")
		return "Counts";

	if (index >= _maxColumn)
		index = _maxColumn - 1;

	char letter = char(97 + index);
	return tq("H₀ (") + letter + tq(")");
}



