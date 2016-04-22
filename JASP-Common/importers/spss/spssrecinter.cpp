
#include "spssrecinter.h"
#include "debug_cout.h"
#include "stringutils.h"
#include <assert.h>
#include <algorithm>

using namespace std;


SPSSColumn::SPSSColumn(const string &nm, const string &lbl, const spss::MissingValueChecker & mssngChckr, long strngLn, int32_t msr)
	: spssLabel(lbl)
	, spssName(nm)
	, _spssStringLen(strngLn)
	, _charsRemaining(strngLn)
	, columnSpan(1)
	, measure(msr)
	, missingChecker(mssngChckr)
{}

/**
 * @brief spssStringLen Find the length of the string (in column).
 * @param value Value to set.
 */
void SPSSColumn::spssStringLen(long value)
{
	_spssStringLen = value;
	_charsRemaining = value;
}

/**
 * @brief spssStringLen Set the length of the string (in column).
 * @return Value
 */
long SPSSColumn::spssStringLen()
const
{
	return _spssStringLen;
}

/**
 * @brief charsRemaining Set the number of chars remaining for this column/case.
 * @param value Value to set.
 */
void SPSSColumn::charsRemaining(long value)
{
	_charsRemaining = value;
}

/**
 * @brief charsRemaining Find the number of chars remaining for this column/case.
 * @return Value
 */
long SPSSColumn::charsRemaining()
const
{
	return _charsRemaining;
}

/**
 * @brief charsRemaining Find the number of chars remaining for this buffer, for one data cell.
 * @param bufferSzie The size of the buffer.
 * @return Value
 */
long SPSSColumn::cellCharsRemaining(size_t bufferSize)
{
	return (_charsRemaining > bufferSize) ? bufferSize : _charsRemaining;
}

/**
 * @brief insert Insert a string into the columns.
 * @param str
 * @return The index of the inserted string.
 */
size_t SPSSColumn::insert(const string &str)
{
	_strings.push_back(str);
	_charsRemaining = _spssStringLen - str.length();
	return _strings.size() - 1;
}

/**
 * @brief append Appends a value to The last inserted string.
 * @param str The string to append.
 * @return index of the string.
 */
size_t SPSSColumn::append(const std::string &str)
{
	size_t index =_strings.size() - 1;
	_strings[index].append(str);
	if (_charsRemaining > str.size())
		_charsRemaining = _charsRemaining - str.size();
	else
		_charsRemaining = 0;
	return index;
}

/**
 * @brief get Gets the string at index.
 * @param index
 * @return
 */
const std::string &SPSSColumn::get(size_t index)
const
{
	return _strings.at(index);
}


SPSSColumns::SPSSColumns()
	: _colCtr(-1)
	, _spanCtr(1)
	, _numCases(-1L)
{
}


/**
 * @brief resetCols Used after vect::push_back() or similar, reset the next col iterator
 *
 */
void SPSSColumns::resetCols()
{
	_colCtr = -1;
	_spanCtr = 1;
}


/**
 * @brief getColumn Get next column wrapping as required.
 * @return
 */
SPSSColumn& SPSSColumns::getNextColumn()
{

	// First time or done spanning the (last) column?
	if ((_colCtr == -1) || (at(_colCtr).columnSpan == _spanCtr))
	{
		// Goto next column..
		_colCtr++;
		_spanCtr = 1;
	}
	else
		// increment the colum spanned count.
		_spanCtr++;

	// off end?
	if (((unsigned) _colCtr) >= size())
	{
		_colCtr = 0;
		_spanCtr = 1;

		// Reset all the string remaining values.
		for (size_t i = 0; i < size(); i++)
		{
			SPSSColumn & col = at(i);
			col.charsRemaining(col.spssStringLen());
		}
	}

	return at(_colCtr);
}

/**
 * @brief isSpanning
 * @return True if the last getColumn() call found a contination column.
 */
bool SPSSColumns::isSpanning() const
{
	return _spanCtr > 1; // Starting value (1) plus one - just found the continuation column.
}


/**
 * @brief numCases Set the number of cases.
 * @param num Number of cases to set.
 */
void SPSSColumns::numCases(int32_t num)
{
	if (_numCases == -1L)
		_numCases = num;
}

void SPSSColumns::numCases(int64_t num)
{
	if (_numCases == -1L)
		_numCases = num;
}

/**
 * @brief processVerLongStrings Appends very long strings together, if present.
 * Call after the data is loaded!.
 */
void SPSSColumns::processVeryLongStrings()
{
	// For every found ver long string.
	const LongColsData &strLens = veryLongColsDat();
	for (map<string, size_t>::const_iterator ituple = strLens.begin(); ituple != strLens.end(); ituple++)
	{
		// find the root col...
		SPSSColumns::iterator rootIter;
		for (rootIter = begin(); rootIter != end(); rootIter++)
		{
#ifndef QT_NO_DEBUG
			DEBUG_COUT5("Matching: \"", rootIter->spssName, "\" with \"", ituple->first, "\"");
#endif
			if (rootIter->spssName == ituple->first)
					break;
		}

		// Shouldn't happen..
		if (rootIter == end())
			DEBUG_COUT2("SPSSColumns::processVeryLongStrings(): Failed to find match for ", ituple->first.c_str());

		DEBUG_COUT3("Found SPSS col ", rootIter->spssName.c_str(), " root to append..");

		while (rootIter->spssStringLen() < ituple->second)
		{
			// Find the next segment, (Should be next one along)
			SPSSColumns::iterator ncol = rootIter;
			ncol++;

			// concatinate all the strings, going down the cases.
			for (size_t cse  = 0; cse < rootIter->strings().size(); cse++)
			{
				// How much more to add?
				long needed = min(ituple->second - rootIter->strings()[cse].size(), rootIter->strings()[cse].size());
				if (needed > 0)
					rootIter->strings()[cse].append(ncol->strings()[cse], 0, needed);
//				DEBUG_COUT9("Appended ", needed, " to \"", ncol->strings()[cse], "\" to ", rootIter->spssName, " for case ", cse, ".");
			}
			rootIter->spssStringLen( rootIter->spssStringLen() + ncol->spssStringLen() );
			// Dump the column.
			erase(ncol);
			// Debug
#ifndef QT_NO_DEBUG
//			for (size_t cse  = 0; cse < rootIter->strings().size(); cse++)
//				DEBUG_COUT7("Found \"", rootIter->strings()[cse], "\" for ", rootIter->spssName, " for case ", cse, ".");
#endif
		}
	}

	// Trim trialing spaces for all strings in the data set.
	for (std::vector<SPSSColumn>::iterator iCol = begin(); iCol != end(); ++iCol)
	{
		if (iCol->isString())
		{
			for (size_t cse  = 0; cse < iCol->strings().size(); cse++)
				StrUtils::rTrimWSIP(iCol->strings()[cse]);
		}
	}

}
