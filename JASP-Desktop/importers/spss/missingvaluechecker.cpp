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
#include "missingvaluechecker.h"
#include "floatinforecord.h"

using namespace std;
using namespace spss;

/**
 * @brief MissingValueChecker Ctor
 * @param n_missing_values The number of missing values (from VariableRecord).
 * @param missing_values The missing values (from VariableRecord).
 */
MissingValueChecker::MissingValueChecker(int32_t n_missing_values, const std::vector<double> & missing_values)
	: _minMissing(DBL_MAX)
	, _maxMissing(DBL_MIN)
{
	// A simple list?
    if (n_missing_values > 0)
	{
	    for (std::vector<double>::const_iterator iter = missing_values.begin();
			 iter != missing_values.end();
			 ++iter)
		{
		    _missing.insert(*iter);
		}
	}
    else if (n_missing_values < -1)
	{ // Min and max?
	    _minMissing = missing_values[0];
	    _maxMissing = missing_values[1];

		// plus on descreet?
	    if (n_missing_values < -1)
		    _missing.insert(missing_values[3]);
	}
}

MissingValueChecker::MissingValueChecker()
	: _minMissing(DBL_MAX)
	, _maxMissing(DBL_MIN)
{
}


MissingValueChecker & MissingValueChecker::operator = (const MissingValueChecker & that)
{
    _minMissing = that._minMissing;
    _maxMissing = that._maxMissing;
    _missing = that._missing;
    return *this;
}

/**
 * @brief isMissingValue represents a missing value.
 * @param fir The Floating point info record.
 * @param value Value to test
 * @return true if a missing value.
 */
bool MissingValueChecker::isMissingValue(const FloatInfoRecord &fir, double value) const
{
	// SYSMIS?
	if ((value == fir.sysmis()) || std::isnan(value))
	    return true;
	// in the range of mssing values?
    else if ((value >= _minMissing) && (value <= _maxMissing))
	    return true;
	// equal to a discreet value?
    else if (_missing.find(value) != _missing.end())
	    return true;
    else
	    return false;
}
