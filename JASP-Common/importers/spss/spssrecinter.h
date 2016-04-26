//
// Copyright (C) 2015-2016 University of Amsterdam
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

#ifndef SPSSRECINTER_H
#define SPSSRECINTER_H

#include "missingvaluechecker.h"
#include "measures.h"
#include "numericconverter.h"
#include <spsscpconvert.h>

#define INDEXED_STRINGS 1

#include <vector>
#include <string>
#ifdef INDEXED_STRINGS
#include <set>
#include <map>
#endif

// If defined the stringsa re vaiable as as indexed values.

namespace spss
{

/**
 * A type that holds the data in an intermate format
 */
class SPSSColumn
{
public:
	std::string spssLabel;		// The name as shown to the user.
	std::string spssName;		// The name as in hte file.

	SPSSColumn(const std::string &name, const std::string &label, const spss::MissingValueChecker &missingChecker, long stringLen, int32_t measure);

private:
	long _spssStringLen;	// The length of this string (-1 for none).
	long _charsRemaining;	// The numer of chars (in this case!) remaind to read-in.
public:

	/**
	 * @brief spssStringLen Find the length of the string (in column).
	 * @param value Value to set.
	 */
	void spssStringLen(long value);

	/**
	 * @brief spssStringLen Set the length of the string (in column).
	 * @return Value
	 */
	long spssStringLen() const;

	/**
	 * @brief charsRemaining Set the number of chars remaining for this column/case.
	 * @param value Value to set.
	 */
	void charsRemaining(long value);

	/**
	 * @brief charsRemaining Find the number of chars remaining for this column/case.
	 * @return Value
	 */
	long charsRemaining() const;

	/**
	 * @brief charsRemaining Find the number of chars remaining for this buffer, for one data cell.
	 * @param bufferSzie The size of the buffer.
	 * @return Value
	 */
	long cellCharsRemaining(size_t bufferSize);

	int columnSpan;
	int32_t measure;
	std::vector<double> numerics;	// one per case.
private:
	std::vector<std::string> _strings; // one per case.
public:

	/**
	 * @brief insert Insert a string into the columns.
	 * @param str
	 * @return The index of the inserted string.
	 */
	size_t insert(const std::string &str);

	/**
	 * @brief append Appends a value to the last inserted string.
	 * @param str The string to append.
	 * @return index of the last inserted string.
	 */
	size_t append(const std::string &str);

	/**
	 * @brief get Gets the string at index.
	 * @param index
	 * @return
	 */
	const std::string &get(size_t index) const;

	/**
	 * @brief strings Get the strings.
	 * @return Reference  to strings.
	 */
	const std::vector<std::string> &strings() const { return _strings; }
	std::vector<std::string> &strings() { return _strings; }

	// A Missing value checker machine.
	spss::MissingValueChecker missingChecker;

	bool isString()
	{
		return measure == spss::Measures::string_type;
	}

};


/*
 * A vector of SPSSColumns, with an "auto iterator."
 * Also holds the convertors for both numeric endian
 * and string code page convertors.
 */
class SPSSColumns : public std::vector<SPSSColumn>
{
public:

	typedef std::map<std::string, size_t> LongColsData;

	SPSSColumns();

	/**
	 * @brief resetCols Used after vect::push_back() or similar, reset the next col iterator
	 *
	 */
	void resetCols();

	/**
	 * @brief getColumn Get next column wrapping as required.
	 * @return
	 */
	SPSSColumn& getNextColumn();

	/**
	 * @brief isSpanning
	 * @return True if the last getColumn() call found a contination column.
	 */
	bool isSpanning() const;


	/**
	 * @brief numCases Set the number of cases.
	 * @param num Number of cases to set.
	 */
	void numCases(int32_t num);
	void numCases(int64_t num);

	/**
	 * @brief numCases
	 * @return The numer of cases found (-1 if notknown.)
	 */
	size_t numCases()
	const
	{
		return (size_t) _numCases;
	}

	/**
	 * @brief hasNoCases Checks for number of cases > 0
	 * @return true if no cases held/found.
	 */
	bool hasNoCases() const
	{
		return _numCases < 1L;
	}

	/**
	 * @brief veryLongColsDat Sets the very long strings data.
	 * @param vlcd The value to set.
	 */
	void veryLongColsDat(const LongColsData &vlcd)
	{
		_longColsDta.insert(vlcd.begin(), vlcd.end());
	}

	/**
	 * @brief veryLongColsDat Gets the very long strings data.
	 * @return The value found.
	 */
	const LongColsData &veryLongColsDat() const
	{
		return _longColsDta;
	}

	/**
	 * @brief processStringsPostLoad - Delas with very Long strings (len > 255) and CP processes all strings.
	 * Call after the data is loaded!.
	 */
	void processStringsPostLoad(boost::function<void (const std::string &, int)> progress);

	/**
	 * @brief numericsConv Access the numeric convertor.
	 * @return
	 */
	NumericConverter &numericsConv() { return _numConvert; }

	/**
	 * @brief setStrCnvrtr Sets the string convertor to use.
	 * @param convtr
	 *
	 * N.B. This class takes over ownership of the instance:
	 * @code
		...
		inst.setStrCnvrtr( new SpssCPConvert(ICUConnnector::dos437) );
		...
	   @endcode
	 */
	void setStrCnvrtr(SpssCPConvert *convtr)
	{
		_stringConvert.reset(convtr);
	}

	/**
	 * @brief stringsConv Gets the string convertor.
	 * @return String convertoer instance.
	 *
	 * N.B. Will "blow-up" (null pointer exception) if not set!
	 */
	SpssCPConvert &stringsConv() { return *_stringConvert.get(); }

private:

	NumericConverter				_numConvert; /** < Numeric Endain fixer. */

    std::auto_ptr<SpssCPConvert>	_stringConvert; /** < Code Page convertor. */

	// Count columns.
	size_t _colCtr;

	// Counter for spanned columns.
	// 1 if not spanning.
	size_t _spanCtr;

	LongColsData	_longColsDta; /** < Very long strings data. */

	int64_t _numCases; /** < Number of cases found to date. */
};

} // end namespace

#endif // SPSSRECINTER_H
