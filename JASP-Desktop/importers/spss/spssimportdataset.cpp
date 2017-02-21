//
// Copyright (C) 2015-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "spssimportdataset.h"
#include "fileheaderrecord.h"
#include "../importerutils.h"
#include "../importerutils.h"
#include <math.h>
#include <QDateTime>

using namespace std;
using namespace spss;

/*********************************************************************
 *
 * class SPSSImportDataSet
 *
 *********************************************************************/

SPSSImportDataSet::SPSSImportDataSet() : _numCases(-1L)
{
}

SPSSImportDataSet::~SPSSImportDataSet()
{
}

/**
 * @brief numCases Set the number of cases.
 * @param num Number of cases to set.
 */
void SPSSImportDataSet::numCases(int32_t num)
{
	if (_numCases == -1L)
		_numCases = num;
}

void SPSSImportDataSet::numCases(int64_t num)
{
	if (_numCases == -1L)
		_numCases = num;
}

int SPSSImportDataSet::rowCount() const
{
	return numCases();
}

void SPSSImportDataSet::add(size_t dictIndex, SPSSImportColumn *column)
{
	_columns.push_back(column);
	_indexToColumnMap.insert( std::pair<size_t, SPSSImportColumn*>(dictIndex, column) );
}

SPSSImportColumn *SPSSImportDataSet::getColumn(size_t entry)
{
	return _indexToColumnMap.find(entry)->second;
}

void SPSSImportDataSet::setColumnMap()
{
	for (ImportColumns::iterator colIt = begin(); colIt != end(); ++colIt)
	{
		SPSSImportColumn* col = dynamic_cast<SPSSImportColumn*>(*colIt);
		const string& name = col->setSuitableName();
		_nameToColMap.insert(make_pair(name, col));
	}
}
