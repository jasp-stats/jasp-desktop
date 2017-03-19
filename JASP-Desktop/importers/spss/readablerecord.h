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

#ifndef READABLERECORD_H
#define READABLERECORD_H


#include "systemfileformat.h"
#include "../importerutils.h"
#include "numericconverter.h"
#include "../spssimporter.h"

#include <set>


namespace spss
{
class SPSSImportDataSet;

/**
  * @brief The ReadableRecord class: Base class for readable objects.
  *
  */

template <RecordTypes recType> class ReadableRecord
{
public:

	/**
	  * @brief ReadableRecord default Ctor : Builds from the input stream
	  * @param type The record type value.
	  *
	  * When Constructing a FileHeaderRecord fileHEader is 0.
	  */
	ReadableRecord(RecordTypes type);

	/**
	  * @brief ReadableRecord default Ctor : Builds from the input stream
	  * @param Converters &fixer Object that knowns about endiness.
	  * @param fileType The record type value, as found in the file.
	  * @param ifstream from - file to read from
	  *
	  */
	ReadableRecord(const NumericConverter &fixer, RecordTypes fileType, SPSSStream &from);

	virtual ~ReadableRecord() {}

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset) = 0;

	static const RecordTypes RECORD_TYPE = recType;

protected:

	/**
	* @brief roundUpTo Rounds val (in bytes) up to an integer multiple of numBits (in bits)
	* @param val The value to reound, expressed in bytes.
	* @param numBits The number to round upto as expressed in bits.
	* @return The rounded value (in bytes)
	*/
	inline size_t roundUpTo(size_t val, size_t numBits = 32);
};


/*
 * Implmementation of  ReadableRecord.
 */
template <RecordTypes rT>
ReadableRecord<rT>::ReadableRecord(RecordTypes fileType)
{
	if (fileType != RECORD_TYPE)
	{
//		DEBUG_COUT5("ReadableRecord::ctor: Expected record type ", RECORD_TYPE, " got type ", fileType, ".");
		throw std::runtime_error("SPSS record type mismatch (non-file).");
	}
}


#ifndef QT_NO_DEBUG
// see https://scaryreasoner.wordpress.com/2009/02/28/checking-sizeof-at-compile-time
// The basic idea is that we attempt to take the sizeof an array with -ve size
// if (condtion) == false.
#define __BUILD_BUG_ON(condition) ((void)sizeof(char[1 - 2*!!(condition)]))

#else
#define __BUILD_BUG_ON(condition)
#endif

template <RecordTypes rT>
ReadableRecord<rT>::ReadableRecord(const NumericConverter &, RecordTypes fileType, SPSSStream &from)
{
	if (!from.good())
	{
//		DEBUG_COUT1("ReadableRecord::ctor File gone bad.");
		throw std::runtime_error("unexpected EOF");
	}
		if (fileType != RECORD_TYPE)
	{
//		DEBUG_COUT5("ReadableRecord::ctor: Expected record type ", RECORD_TYPE, " got type ", fileType, ".");
		throw std::runtime_error("SPSS record type mismatch.");
	}

	// Paranoid error checking.
	// The data types we use must be the right size before
	// these macro expansions will compile (to nothing).
	__BUILD_BUG_ON(sizeof(double) != 8);
	__BUILD_BUG_ON(sizeof(int32_t) != 4);
	__BUILD_BUG_ON(sizeof(char) != 1);
	__BUILD_BUG_ON(sizeof(Char_3) != 3);
	__BUILD_BUG_ON(sizeof(Char_4) != 4);
	__BUILD_BUG_ON(sizeof(Char_8) != 8);
	__BUILD_BUG_ON(sizeof(Char_9) != 9);
	__BUILD_BUG_ON(sizeof(Char_60) != 60);
	__BUILD_BUG_ON(sizeof(Char_64) != 64);
	__BUILD_BUG_ON(sizeof(Char_80) != 80);
	__BUILD_BUG_ON(sizeof(SpssDataCell) != 8);
}

#undef BUILD_BUG_ON


/**
* @brief roundUpTo Rounds val (in bytes) up to an integer multiple of numBits (in bits)
* @param val The value to reound, expressed in bytes.
* @param numBits The number to round upto as expressed in bits.
* @return The rounded value (in bytes)
*/
template <RecordTypes rT>
inline size_t ReadableRecord<rT>::roundUpTo(size_t val, size_t numBits)
{
	size_t numBts = (numBits / 8);
	if ((val % numBts) == 0)
		return val;
	else
		return val + (numBts - (val % numBts));
}

}

#endif // READABLERECORD_H
