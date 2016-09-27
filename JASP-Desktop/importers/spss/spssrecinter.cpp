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

#include "fileheaderrecord.h"
#include "spssrecinter.h"
#include "debug_cout.h"
#include "stringutils.h"
#include <assert.h>
#include <algorithm>

#include <math.h>


#include <QDateTime>

using namespace std;
using namespace spss;

const QDate SPSSColumn::_beginEpoch(1582, 10, 14);

/*********************************************************************
 *
 * class SPSSColumn
 *
 *********************************************************************/
/**
 * @brief SPSSColumn Ctor
 * @param name SPSS column name (short form)
 * @param label Column label.
 * @param stringLen String length or zero.
 * @param formattype The format Type from the variable record.
 * @param missingChecker Check for missing value with this.
 */
SPSSColumn::SPSSColumn(const std::string &name, const std::string &label, long stringLen, FormatTypes formattype, const spss::MissingValueChecker &missingChecker)
	: _spssColumnLabel(label)
	, _spssRawColName(name)
	, _spssStringLen(stringLen)
	, _columnSpan(1)
	, _spssMeasure(measure_undefined)
	, _spssFormatType(formattype)
	, _missingChecker(missingChecker)
	, _charsRemaining(stringLen)
{

}

SPSSColumn::~SPSSColumn()
{

}

const string &SPSSColumn::spssColumnLabel() const
{
	if (_spssColumnLabel.length() > 0)
		return _spssColumnLabel;
	else if (_spssLongColName.length() > 0)
		return _spssLongColName;
	else
		return _spssRawColName;
}

/**
 * @brief spssStringLen Find the length of the string (in column).
 * @param value Value to set.
 */
void SPSSColumn::spssStringLen(size_t value)
{
	_spssStringLen = value;
	_charsRemaining = value;
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
 * @brief getJaspColumnType Finds the column type that JASP will use.
 * @return A column type.
 *
 */
Column::ColumnType SPSSColumn::getJaspColumnType() const
{
	switch(spssFormatType())
	{
	case format_A:
	case format_AHEX:
		return Column::ColumnTypeNominalText;	// Strings are nominal text.

	// Date and time formats are converted to
	// strings, when read in.
	case format_TIME:
	case format_DTIME:
	case format_DATE:
	case format_ADATE:
	case format_EDATE:
	case format_JDATE:
	case format_SDATE:
	case format_QYR:
	case format_MOYR:
	case format_WKYR:
	case format_DATETIME:
	case format_WKDAY:
	case format_MONTH:
		return Column::ColumnTypeNominalText;

	default: // Everything else is a number of some sort.

		switch(spssMeasure())
		{
		case measure_continuous:
		case measure_undefined:
		case measure_spss_unknown:
			// If we know no better, then it is a FP.
			return Column::ColumnTypeScale;
		case measure_nominal:
			return Column::ColumnTypeNominal;
		case measure_ordinal:
			return Column::ColumnTypeOrdinal;
		}
	}
	return Column::ColumnTypeScale;
}



/**
 * @brief _toQDateTime Convert SPSS seconds to a date/time.
 * @param dt - Target
 * @param seconds Number of seconds since start of SPSS epoch.
 * @return void
 */
QDateTime* SPSSColumn::_asDateTime(double seconds)
{
	qint64 totalSecs = floor(seconds);
	qint64 days = totalSecs / (24*3600);
	qint64 secs = totalSecs % (24*3600);

	QDate dt = QDate::fromJulianDay(days + _beginEpoch.toJulianDay());

	int hours = secs / 3600;
	secs = secs % 3600;
	int mins = secs / 60;
	secs = secs % 60;
	QTime tm(hours, mins, (int) secs);

	return new QDateTime(dt, tm, Qt::UTC);
}


QString SPSSColumn::_weekday(unsigned short int wd)
const
{
	switch(wd)
	{
	case 1:
		return "Sunday";
	case 2:
		return "Monday";
	case 3:
		return "Tuesday";
	case 4:
		return "Wednesday";
	case 5:
		return "Thursday";
	case 6:
		return "Friday";
	case 7:
		return "Saturday";
	default:
		return "";
	}
}

QString SPSSColumn::_month(unsigned short int mnth)
const
{
	switch(mnth)
	{
	case 1:
		return "January";
	case 2:
		return "February";
	case 3:
		return "March";
	case 4:
		return "April";
	case 5:
		return "May";
	case 6:
		return "June";
	case 7:
		return "July";
	case 8:
		return "August";
	case 9:
		return "September";
	case 10:
		return "October";
	case 11:
		return "November";
	case 12:
		return "December";
	default:
		return "";
	}
}


/**
 * @brief format Fomats a number that SPSS holds as a numeric value that JASP cannot deal with.
 * @param value The value to format.
 * @param floatInfo The float info record we have.
 * @return Formatted date, or "" (string empty) for no value (0)
 */
string SPSSColumn::format(double value, const FloatInfoRecord &floatInfo) const
{
	QString result;
	if (!std::isnan(value))
	{
		QDateTime * dt = _asDateTime(value);

		switch(spssFormatType())
		{
		case format_A:
		case format_AHEX:
		default:
			break;

		case format_F:
		case format_COMMA:
		case format_DOT:
		case format_DOLLAR:
		case format_PCT:
		case format_E:
		case format_CCA:
		case format_CCB:
		case format_CCC:
		case format_CCD:
		case format_CCE:
		case format_N:
			result = (missingChecker().isMissingValue(floatInfo, value)) ? QString() : QString::number(value);
			break;

		// Date and time formats are converted to
		// strings, when read in.
		case format_TIME:
			result = dt->time().toString("hh:mm:ss");
			break;

		case format_DTIME:
			result = QString("%1").arg(dt->date().toJulianDay() - _beginEpoch.toJulianDay(), 2, QLatin1Char('0'));
			result.append(dt->time().toString(" hh:mm:ss"));
			break;

		case format_DATE:
			result = dt->toString("dd-MMM-yyyy");
			break;
		case format_ADATE:
			result = dt->toString("MM-dd-yyyy");
			break;
		case format_EDATE:
			result = dt->toString("dd.MM.yyyy");
			break;
		case format_JDATE:
			result = dt->date().toString("yyyy");
			result.append(QString("%1").arg(dt->date().daysInYear(), 3, 10, QLatin1Char('0')));
			break;
		case format_SDATE:
			result = dt->toString("yyyy/MM/dd");
			break;
		case format_QYR:
			result = QString::number(dt->date().month() % 4);
			result.append(dt->toString(" Q yyyy"));
			break;
		case format_MOYR:
			result = dt->toString("MMM yyyy");
			break;
		case format_WKYR:
			result.append(QString("%1").arg((dt->date().dayOfYear() / 7) + 1, 2, 10, QLatin1Char('0')));
			result.append(dt->toString(" WK yyyy"));
			break;

		case format_DATETIME:
			result = dt->toString("dd-MMM-yyyy hh:mm:ss");
			break;
		case format_WKDAY:
			result = _weekday(static_cast<unsigned short int>(floor(value)));
			break;
		case format_MONTH:
			result = _month(static_cast<unsigned short int>(floor(value)));
			break;
		}

		delete dt;
	}

	return static_cast<const char *>(result.toUtf8());
}

/**
 * @brief processStrings Converts any strings in the data fields.
 * @param dictData The
 *
 * Should be implemented in classes where holdStrings maybe or is true.
 *
 */
void SPSSColumn::processStrings(const CodePageConvert &converter)
{
	_spssColumnLabel = converter.convertCodePage(_spssColumnLabel);
	_spssRawColName = converter.convertCodePage(_spssRawColName);
	_spssLongColName = converter.convertCodePage(_spssLongColName);

	// Strings are converted elsewhere,
	// since, we convert some data types (dates etc.) on the fly to UTF-8 strings.
	{
		LabelByValueDict result;
		for (LabelByValueDict::iterator i = spssLables.begin(); i != spssLables.end(); ++i)
			result.insert( LabelByValueDictEntry(i->first, converter.convertCodePage(i->second)) );
		spssLables = result;
	}
}



/**
 * @brief containsFraction Returns false if all values are integer.
 * @param values VAlues to check
 * @return true if a fractional part found.
 */
bool SPSSColumn::_containsFraction(const std::vector<double> &values)
{
	for (std::vector<double>::const_iterator i = values.begin();
		 i != values.end();
		 ++i)
	{
		if (std::isnan(*i) == false)
		{
			double intprt;
			double fracprt = modf(*i, &intprt);
			if (fracprt != 0.0)
				return true;
		}
	}
	return false;
}


/**
 * @brief cellType Gets the cell data type we expect to read for this coloumn.
 * @return
 */
enum SPSSColumn::e_celTypeReturn SPSSColumn::cellType() const
{
	switch(_spssFormatType)
	{
	case format_A:
	case format_AHEX:
		return cellString;
	default:
		return cellDouble;
	}
}


/**
 * @brief insert Insert a string into the columns.
 * @param str
 * @return The index of the inserted string.
 */
size_t SPSSColumn::insert(const string &str)
{
	if (cellType() == cellString)
	{
		strings.push_back(str);
		_charsRemaining = _spssStringLen - str.length();
		return strings.size() - 1;
	}
	else
		return 0;
}

/**
 * @brief append Appends a value to The last inserted string.
 * @param str The string to append.
 * @return index of the string.
 */
size_t SPSSColumn::append(const std::string &str)
{
	if (cellType() == cellString)
	{
		size_t index = strings.size() - 1;
		strings[index].append(str);
		if (_charsRemaining > str.size())
			_charsRemaining = _charsRemaining - str.size();
		else
			_charsRemaining = 0;
		return index;
	}
	else
		return 0;
}

/**
 * @brief get Gets the string at index.
 * @param index
 * @return
 */
const std::string &SPSSColumn::getString(size_t index)
const
{
	static const string empty;

	if (cellType() == cellString)
		return strings.at(index);
	else
		return empty;
}


/*********************************************************************
 *
 * class SPSSColumns
 *
 *********************************************************************/

SPSSColumns::SPSSColumns()
	: _numCases(-1L)
	, _currentColIter(SPSSDictionary::end())
	, _remainingColSpan(0)
	, _isSpaning(false)
{
}


/**
 * @brief resetCols Used after vect::push_back() or similar, reset the next col iterator
 *
 */
void SPSSColumns::resetCols()
{
	_currentColIter = SPSSDictionary::begin();
	_remainingColSpan = _currentColIter->second.columnSpan();
}


/**
 * @brief getColumn Get next column wrapping as required.
 * @return
 */
SPSSColumn& SPSSColumns::getNextColumn()
{
	// Grab what will be the result.
	SPSSColumn &result = _currentColIter->second;

	// Set the spaning var.
	_isSpaning = result.columnSpan() != _remainingColSpan;

//	DEBUG_COUT11("Returning col. \"", result.spssColumnLabel(), "\"/\"", result.spssColumnName(), "\", spanning ", ((_isSpaning) ? "true" : "false"), " ", result.columnSpan(), ", with ", _remainingColSpan, " remaining.");

	// Anthing left (for next time)?
	if (--_remainingColSpan == 0)
	{
		// Go to next column.
		++_currentColIter;
		// Dropped off end?
		if (_currentColIter == SPSSDictionary::end())
			resetCols();
		else
			_remainingColSpan = _currentColIter->second.columnSpan();
	}
	return result;
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
 * @brief processStringsPostLoad - Deals with very Long strings (len > 255) and CP processes all strings.
 * Call after the data is loaded!.
 */
void SPSSColumns::processStringsPostLoad(boost::function<void (const std::string &, int)> progress)
{
	// For every found very long string.
	const LongColsData &strLens = veryLongColsDat();
	float numStrlens = distance(strLens.begin(), strLens.end());
	for (map<string, size_t>::const_iterator ituple = strLens.begin(); ituple != strLens.end(); ituple++)
	{
		{ // report progress
			float prog = 100.0 * ((float) distance(strLens.begin(), ituple)) / numStrlens;
			static float lastProg = -1.0;
			if ((prog - lastProg) >= 1.0)
			{
				progress("Processing long strings.", (int) (prog + 0.5));
				lastProg = prog;
			}

		}
		// find the root col...
		SPSSColumns::iterator rootIter;
		for (rootIter = begin(); rootIter != end(); rootIter++)
		{
			DEBUG_COUT7("Matching '", ituple->first, "' against '", rootIter->second.spssRawColName(), "' size ", ituple->second, ".");
			if (rootIter->second.spssRawColName() == ituple->first)
					break;
		}

		const long mergedStrlen = 252;
		// Shouldn't happen..
		if (rootIter == end())
			throw runtime_error("Failed to process a very long string value.");

		// merged strings are slightly shorter than what is in the file.
		// See Appendix B System File Format, PSPP devloper's Guide, release 0.10.2 pp69
		if (rootIter->second.spssStringLen() == 255)
			rootIter->second.spssStringLen(mergedStrlen);

		// Chop the length of the strings in the root col.
		for (size_t cse  = 0; cse < rootIter->second.strings.size(); cse++)
		{
			string str = rootIter->second.strings[cse].substr(0, rootIter->second.spssStringLen());
			rootIter->second.strings[cse] = str;
		}

		while (rootIter->second.spssStringLen() < ituple->second)
		{
			// Find the next segment, (Should be next one along)
			SPSSColumns::iterator ncol = rootIter;
			++ncol;

			// How much to fetch?
			long needed = min(ituple->second - rootIter->second.spssStringLen(), rootIter->second.spssStringLen());
			needed = min(mergedStrlen, needed);

			// Concatinate all the strings, going down the cases.
			for (size_t cse  = 0; cse < rootIter->second.strings.size(); cse++)
			{
				if (needed > 0)
					rootIter->second.strings[cse].append(ncol->second.strings[cse], 0, needed);
			}
			// Advance the string length.
			rootIter->second.spssStringLen( rootIter->second.spssStringLen() + needed );
			// Dump the column.
			erase(ncol);
		}
	}

	// Trim trialing spaces for all strings in the data set.
	size_t numCols = distance(begin(), end());
	for (SPSSDictionary::iterator iCol = begin(); iCol != end(); ++iCol)
	{
		{ // report progress
			float prog = 100.0 * ((float) distance(begin(), iCol)) / numCols;
			static float lastProg = -1.0;
			if ((prog - lastProg) >= 1.0)
			{
				progress("Processing strings.", (int) (prog + 0.5));
				lastProg = prog;
			}
		}

		if (iCol->second.cellType() == SPSSColumn::cellString)
		{
			DEBUG_COUT3("Dumping column '", iCol->second.spssRawColName(), "'.");
			for (size_t cse  = 0; cse < iCol->second.strings.size(); cse++)
			{
				// Trim left and right.
				StrUtils::lTrimWSIP(iCol->second.strings[cse]);
				StrUtils::rTrimWSIP(iCol->second.strings[cse]);
			}
		}
	}
}
