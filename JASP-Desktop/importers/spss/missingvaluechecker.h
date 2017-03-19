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

#ifndef MISSINGVALUECHECKER_H
#define MISSINGVALUECHECKER_H

#include <set>
#include <vector>
#include <cfloat>
#include <stdint.h>
#include <math.h>

namespace spss {


class FloatInfoRecord;

/**
 * @brief The MissingValueChecker class Tests for a missing value.
 *
 */
class MissingValueChecker
{
public:
	/**
	 * @brief MissingValueChecker Ctor
	 * @param n_missing_values The number of missing values (from VariableRecord).
	 * @param missing_values The missing values (from VariableRecord).
	 */
	MissingValueChecker(int32_t n_missing_values, const std::vector<double> & missing_values);
	MissingValueChecker();

	MissingValueChecker & operator = (const MissingValueChecker & that);

	/**
	 * @brief isMissingValue represents a missing value.
	 * @param fir The Floating point info record.
	 * @param value Value to test
	 * @return true if a missing value.
	 */
	bool isMissingValue(const FloatInfoRecord &fir, double value) const;

	/**
	 * @brief processMissingValue Checks a processes a value for missing.
	 * @param fir The Floating point info record.
	 * @param value The value to test
	 * @param jasp_missing The value returned if \code value \endcode is a missing value.
	 * @return \code jasp_missing \endcode if missing value, \code value \endcode if not.
	 */
	double processMissingValue(const FloatInfoRecord &fir, double value, double jasp_missing = NAN)
	const
	{ return (isMissingValue(fir, value)) ? jasp_missing : value; }

private:

	double  _minMissing;	   // The lower end of the range of missing values.
	double  _maxMissing;	   // The upper end of the range of mssing values.

	std::set<double> _missing; // A discreet set of missing values.

};

} // End name space PSPP

#endif // MISSINGVALUECHECKER_H
