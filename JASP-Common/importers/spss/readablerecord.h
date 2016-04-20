#ifndef READABLERECORD_H
#define READABLERECORD_H


#include "systemfileformat.h"
#include "debug_cout.h"
#include "numericconverter.h"
#include "spssrecinter.h"

#include <set>

namespace spss
{

	class RecordRoot
	{
	public:

		/**
		  * @brief RecordRoot default Ctor
		  *
		  * When Constructing a FileHeaderRecord fileHEader is 0.
		  */
		RecordRoot();

		~RecordRoot();

		/**
		 * @brief processStrings Converts any strings in the data fields.
		 * @param dictData The
		 *
		 * Should be implemented in classes where holdStrings maybe or is true.
		 *
		 */
		virtual void processStrings(const SpssCPConvert &converter) {};

		/**
		 * @brief processAllStrings Calls processStrings(const SpssCPConvert) on all memeber of _stringholders.
		 * @param converter The convertor to pass on.
		 */
		static void processAllStrings(const SpssCPConvert &converter);

	protected:
		static std::set<RecordRoot *> * _pRecords; /** < Holds all instances where holdsStrings == true */
	};

/**
  * @brief The ReadableRecord class: Base class for readable objects.
  *
  */

template <RecordTypes recType>
class ReadableRecord : public RecordRoot
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
	virtual void process(SPSSColumns & columns) = 0;\

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
		DEBUG_COUT5("ReadableRecord::ctor: Expected record type ", RECORD_TYPE, " got type ", fileType, ".");
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
	: RecordRoot()
{
	if (!from.good())
	{
		DEBUG_COUT1("ReadableRecord::ctor File gone bad.");
		throw std::runtime_error("unexpected EOF");
	}
		if (fileType != RECORD_TYPE)
	{
		DEBUG_COUT5("ReadableRecord::ctor: Expected record type ", RECORD_TYPE, " got type ", fileType, ".");
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
