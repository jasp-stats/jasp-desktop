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

#ifndef _NUMERICCONVERTER_H__
#define _NUMERICCONVERTER_H__

#include "systemfileformat.h"

#include <stdint.h>
#include <stddef.h>

namespace spss
{

/**
 * @brief The Converters class
 *
 * Holds flags and performs actions on 32 bit is and doubles.
 * In addtion it performs anaylsis of values determining
 * what action needs to be taken.
 */
class NumericConverter
{
public:

	typedef enum e_fpTypes
	{
		fp_unknown = 0,
		fp_IBM370 = -2,		// IBM 360 though IBM z series?
		fp_DECVAX_E = -1,	// DEC VAXen
		fp_IEEE754 = 1,		// IEEE 754
	} FPTypes;

	typedef enum e_endain
	{
		mach_unknown = 0,
		mach_bigEndian = -1,
		mach_littleEndian = 1
	} Endians;

	NumericConverter();

	/**
	 * @brief setFPType Sets the FP type.
	 * @param type FP type to set.
	 */
	void setFPType(FPTypes type);

	/**
	 * @brief Sets the Endainness.
	 * @param type Type to set.
	 */
	void setEndian(Endians type);

	/**
	 * @brief analyse Deterimines the endin order of value.
	 * @param value Value to analyse.
	 * @param expectedValues Zero terminated array of expected values.
	 */
	void analyse(int32_t value, const int32_t *expectedValues);

	/**
	 * @brief analyse Deterimines the endin order of value.
	 * @param value Value to analyse.
	 * @param expectedValues Zero terminated array of expected values.
	 */
	void analyse(double value, const double *expectedValues);

	/**
	 * @brief Fixes the passed value in place.
	 * @param value Value to fix
	 */
	inline void fixup(int32_t *value) const;
	inline void fixup(int64_t *value) const;

	/**
	 * @brief Fixes the passed value in place.
	 * @param value Value to fix
	 */
	inline void fixup(double *value) const;

	/**
	 * A dummy fixups for strings
	 * CodePage converstion is done elsewhere.
	 */
	inline void fixup(char *, size_t) const {}
	inline void fixup(Char_3 *) const {}
	inline void fixup(Char_4 *) const {}
	inline void fixup(Char_8 *) const {}
	inline void fixup(Char_9 *) const {}
	inline void fixup(Char_60 *) const {}
	inline void fixup(Char_64 *) const {}
	inline void fixup(Char_80 *) const {}

	/**
	 * @brief Makes the double endinnes the same as ints
	 *
	 */
	void forceLocalSystemDbl() { _fpType = _fpThisMachine; }


private:

	// Unions for the byteswaps.
	typedef union u_intBytes
	{
		int32_t	*i;
		int8_t	*c;
	} IntBytes;
	typedef union u_bintBytes
	{
		int64_t	*i;
		int8_t	*c;
	} BIntBytes;
	typedef union u_dblBytes
	{
		double	*i;
		int8_t	*c;
	} DbleBytes;

	/**
	 * Template function to check if the passed value is in the list.
	 */
	template <class T>
	bool _isInExpected(T value, const T *expectedValues);

	Endians _endSource;	/** < Endain from analysis / setters. */
	FPTypes _fpType;	/** Floating point number. */
	const FPTypes _fpThisMachine = fp_IEEE754;

/*
 * Define the endian order the target machine uses.
 */
#if defined(__BYTE_ORDER__) && defined(__ORDER_LITTLE_ENDIAN__) && (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
	const Endians _endThisMachine = mach_littleEndian;
#elif defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
	const Endians _endThisMachine = mach_bigEndian;
#else
  #error "Could not determine target system endian byte order."
#endif
};

/*
 * These functions are inlined for speed.
 */
inline void NumericConverter::fixup(int32_t *value) const
{
	assert( _endSource != mach_unknown );
	if ( _endSource != _endThisMachine )
	{
		IntBytes v;
		v.i = value;
		const size_t lastval = sizeof(int32_t) - 1;
		for (size_t j = 0; j < (sizeof(int32_t) / 2); j++)
		{
			char x = v.c[j];
			v.c[j] = v.c[lastval - j];
			v.c[lastval - j] = x;
		}
	}
}

inline void NumericConverter::fixup(int64_t *value) const
{
	assert( _endSource != mach_unknown );
	if ( _endSource != _endThisMachine )
	{
		BIntBytes v;
		v.i = value;
		const size_t lastval = sizeof(int64_t) - 1;
		for (size_t j = 0; j < (sizeof(int64_t) / 2); j++)
		{
			char x = v.c[j];
			v.c[j] = v.c[lastval - j];
			v.c[lastval - j] = x;
		}
	}
}


inline void NumericConverter::fixup(double *value) const
{
	assert (_fpType == fp_IEEE754);
	assert( _endSource != mach_unknown );
	if ( _endSource != _endThisMachine )
	{
		DbleBytes v;
		v.i = value;
		const size_t lastval = sizeof(double) - 1;
		for (size_t j = 0; j < (sizeof(double) / 2); j++)
		{
			char x = v.c[j];
			v.c[j] = v.c[lastval - j];
			v.c[lastval - j] = x;
		}
	}
}

/**
 * Template function to check if the passed value is in the list.
 */
template <class T>
bool NumericConverter::_isInExpected(T value, const T *expectedValues)
{
	const T *ep = expectedValues;
	while ( (*ep != 0 ) && (value != *ep) )
		ep++;
	return (*ep != 0);
}


} // end namespace spss
#endif // Converters_H
