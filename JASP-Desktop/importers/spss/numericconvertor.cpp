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

#include "systemfileformat.h"
#include "numericconverter.h"
#include <stdexcept>

using namespace std;
using namespace spss;

NumericConverter::NumericConverter()
	: _endSource(mach_unknown)
	, _fpType(fp_unknown)
{
}


/**
 * @brief setFPType Sets the FP type.
 * @param type FP type to set.
 */
void NumericConverter::setFPType(FPTypes type)
{
	const char * msg;

	switch(type)
	{
	case fp_IEEE754:
		_fpType = type;
		return;

	case fp_DECVAX_E:
		msg = "DEC VAX";
		break;
	case fp_IBM370:
		msg = "IBM mainframe";
		break;
	case fp_unknown:
		msg = "unknown origin";
		break;
	}
	throw runtime_error(string("Cannot read (floating point) numbers from ") + msg + " in file.");
}

/**
 * @brief Sets the Endainness.
 * @param type Type to set.
 */
void NumericConverter::setEndian(Endians type)
{
	switch (type)
	{
	case mach_bigEndian:
	case mach_littleEndian:
		_endSource = type;
		return;

	default:
		throw runtime_error("Cannot figure out byte order in file.");
	}
}


/**
 * @brief analyse Deterimines the endin order of value.
 * @param value Value to analyse.
 * @param expectedValues Zero terminated array of expected values.
 */
void NumericConverter::analyse(int32_t value, const int32_t *expectedValues)
{

	// Check for unmodified...
	if (_isInExpected(value, expectedValues))
		setEndian(_endThisMachine);
	else
	{
		// See if we can find it's reversed value.
		setEndian((_endThisMachine == mach_littleEndian) ? mach_bigEndian : mach_littleEndian);
		fixup(&value);
		if (_isInExpected(value, expectedValues) == false)
			setEndian(mach_unknown); // Throws an exception.
	}
}

/**
 * @brief analyse Deterimines the endin order of value.
 * @param value Value to analyse.
 * @param expectedValues Zero terminated array of expected values.
 */
void NumericConverter::analyse(double value, const double *expectedValues)
{
	// NB This simple implmneation only searches for IEEE floating point values.

	// Check for endinnss?...
	if (_endSource != mach_unknown)
		// No, Simply verify the value is recognisable..
		setFPType( (_isInExpected(value, expectedValues)) ? _fpThisMachine : fp_unknown );
	else
	{
		// Try for other endinness.
		setEndian((_endThisMachine == mach_littleEndian) ? mach_bigEndian : mach_littleEndian);
		fixup(&value);
		analyse(value, expectedValues);
	}
}
