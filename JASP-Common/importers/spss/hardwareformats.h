#ifndef HARDWAREFORMATS_H
#define HARDWAREFORMATS_H

#include <stdint.h>
#include <stddef.h>
#include <endian.h>

#include "systemfileformat.h"

namespace spss
{

/**
 * @brief The HardwareFormats class
 *
 * Holds flags and performs actions on 32 bit is and doubles.
 * In addtion it performs anaylsis of values determining
 * what action needs to be taken.
 */
class HardwareFormats
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

	HardwareFormats();

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
	 * Template function to check if teh passed value is in the list.
	 */
	template <class T>
	bool _isInExpected(T value, const T *expectedValues);

	Endians _endSource;	/** < Endain from analysis / setters. */
	FPTypes _fpType;	/** Floating point number. */
	const FPTypes _fpThisMachine = fp_IEEE754;
#if defined(__BYTE_ORDER) && defined(__LITTLE_ENDIAN) && (__BYTE_ORDER == __LITTLE_ENDIAN)
	const Endians _endThisMachine = mach_littleEndian;
#elif defined(__BYTE_ORDER) && defined(__BIG_ENDIAN) && (__BYTE_ORDER == __BIG_ENDIAN)
	const Endians _endThisMachine = mach_bigEndian;
#else
  #error "Could not determine target system byte order."
#endif
};

/*
 * These functions are inlined for speed.
 */
inline void HardwareFormats::fixup(int32_t *value) const
{
	assert( _endSource != mach_unknown );
	if ( _endSource != _endThisMachine )
	{
		IntBytes v;
		v.i = value;
		const size_t lastval = sizeof(int32_t) - 1;
		for (register size_t j = 0; j < (sizeof(int32_t) / 2); j++)
		{
			register char x = v.c[j];
			v.c[j] = v.c[lastval - j];
			v.c[lastval - j] = x;
		}
	}
}

inline void HardwareFormats::fixup(int64_t *value) const
{
	assert( _endSource != mach_unknown );
	if ( _endSource != _endThisMachine )
	{
		BIntBytes v;
		v.i = value;
		const size_t lastval = sizeof(int64_t) - 1;
		for (register size_t j = 0; j < (sizeof(int64_t) / 2); j++)
		{
			register char x = v.c[j];
			v.c[j] = v.c[lastval - j];
			v.c[lastval - j] = x;
		}
	}
}


inline void HardwareFormats::fixup(double *value) const
{
	assert (_fpType == fp_IEEE754);
	assert( _endSource != mach_unknown );
	if ( _endSource != _endThisMachine )
	{
		DbleBytes v;
		v.i = value;
		const size_t lastval = sizeof(double) - 1;
		for (register size_t j = 0; j < (sizeof(double) / 2); j++)
		{
			register char x = v.c[j];
			v.c[j] = v.c[lastval - j];
			v.c[lastval - j] = x;
		}
	}
}

/**
 * Template function to check if the passed value is in the list.
 */
template <class T>
bool HardwareFormats::_isInExpected(T value, const T *expectedValues)
{
	const T *ep = expectedValues;
	while ( (*ep != 0 ) && (value != *ep) )
		ep++;
	return (*ep != 0);
}


} // end namespace spss
#endif // HARDWAREFORMATS_H
