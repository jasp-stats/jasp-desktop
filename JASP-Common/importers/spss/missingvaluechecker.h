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
	bool isMissingValue(const FloatInfoRecord &fir, double value);

	/**
	 * @brief processMissingValue Checks a processes a value for missing.
	 * @param fir The Floating point info record.
	 * @param value The value to test
	 * @param jasp_missing The value returned if \code value \endcode is a missing value.
	 * @return \code jasp_missing \endcode if missing value, \code value \endcode if not.
	 */
	double processMissingValue(const FloatInfoRecord &fir, double value, double jasp_missing = NAN)
	{ return (isMissingValue(fir, value)) ? jasp_missing : value; }

private:

	double  _minMissing;	   // The lower end of the range of missing values.
	double  _maxMissing;	   // The upper end of the range of mssing values.

	std::set<double> _missing; // A discreet set of missing values.

};

} // End name space PSPP

#endif // MISSINGVALUECHECKER_H
