#include "spssimportcolumn.h"
#include "spssimportdataset.h"

using namespace std;
using namespace spss;

const QDate SPSSImportColumn::_beginEpoch(1582, 10, 14);

/*********************************************************************
 *
 * class SPSSImportColumn
 *
 *********************************************************************/
/**
 * @brief SPSSImportColumn Ctor
 * @param name SPSS column name (short form)
 * @param label Column label.
 * @param stringLen String length or zero.
 * @param formattype The format Type from the variable record.
 * @param missingChecker Check for missing value with this.
 */
SPSSImportColumn::SPSSImportColumn(SPSSImportDataSet *spssdataset, const std::string &name, const std::string &label, long stringLen, FormatTypes formattype, const spss::MissingValueChecker &missingChecker)
	: ImportColumn(label)
	, _spssColumnLabel(label)
	, _spssRawColName(name)
	, _spssStringLen(stringLen)
	, _columnSpan(1)
	, _spssMeasure(measure_undefined)
	, _spssFormatType(formattype)
	, _missingChecker(missingChecker)
	, _charsRemaining(stringLen)
	, _dataset(spssdataset)
{

}

SPSSImportColumn::~SPSSImportColumn()
{

}

size_t SPSSImportColumn::size() const
{
	if (cellType() == cellString)
		return strings.size();
	else
		return numerics.size();
}

bool SPSSImportColumn::isValueEqual(Column &col, size_t row) const
{
	if (row >= size())
		return false;

	bool result = false;
	switch (col.columnType())
	{
		case Column::ColumnTypeScale:
			if (row < numerics.size())
			{
				double doubleValue = missingChecker().processMissingValue(_dataset->getFloatInfo(), numerics.at(row));
				result = col.isValueEqual(row, doubleValue);
			}
			break;

		case Column::ColumnTypeNominal:
		case Column::ColumnTypeOrdinal:
			if (row < numerics.size())
			{
				int intValue = INT_MIN;
				if (missingChecker().isMissingValue(_dataset->getFloatInfo(), numerics[row]) == false)
					intValue = static_cast<int>(numerics[row]);
				result = col.isValueEqual(row, intValue);
				break;
			}
			break;
		default:
			if (cellType() == cellString)
			{
				if (row < strings.size())
				{
					CodePageConvert &strConvertor = _dataset->stringsConv();
					string strValue = strConvertor.convertCodePage(strings[row]);
					result = col.isValueEqual(row, strValue);
				}
			}
			else
			{
				if (row < numerics.size())
				{
					string strValue = format(numerics[row], _dataset->getFloatInfo());
					result = col.isValueEqual(row, strValue);
				}
			}
			break;
	}

	return result;
}

const string& SPSSImportColumn::setSuitableName()
{
    if (_spssLongColName.length() > 0)
		_name = _spssLongColName;
    else if (_spssColumnLabel.length() > 0)
        _name = _spssColumnLabel;
    else
		_name = _spssRawColName;

	return _name;
}

/**
 * @brief spssStringLen Find the length of the string (in column).
 * @param value Value to set.
 */
void SPSSImportColumn::spssStringLen(size_t value)
{
	_spssStringLen = value;
	_charsRemaining = value;
}


/**
 * @brief charsRemaining Find the number of chars remaining for this buffer, for one data cell.
 * @param bufferSzie The size of the buffer.
 * @return Value
 */
long SPSSImportColumn::cellCharsRemaining(size_t bufferSize)
{
	return (_charsRemaining > bufferSize) ? bufferSize : _charsRemaining;
}

/**
 * @brief getJaspColumnType Finds the column type that JASP will use.
 * @return A column type.
 *
 */
Column::ColumnType SPSSImportColumn::getJaspColumnType() const
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
QDateTime* SPSSImportColumn::_asDateTime(double seconds)
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


QString SPSSImportColumn::_weekday(unsigned short int wd)
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

QString SPSSImportColumn::_month(unsigned short int mnth)
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
string SPSSImportColumn::format(double value, const FloatInfoRecord &floatInfo) const
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
void SPSSImportColumn::processStrings(const CodePageConvert &converter)
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
bool SPSSImportColumn::_containsFraction(const std::vector<double> &values)
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
enum SPSSImportColumn::e_celTypeReturn SPSSImportColumn::cellType() const
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
size_t SPSSImportColumn::insert(const string &str)
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
size_t SPSSImportColumn::append(const std::string &str)
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
 * @brief setColumnScaleData Sets floating point data into the column.
 * @param column The columns to insert into.
 */
void SPSSImportColumn::setColumnScaleData(Column &column)
{
	vector<double> values = numerics;
	size_t numCases = _dataset->numCases();
	if (values.size() > numCases)
		values.resize(numCases);
	// Process any NAN values.
	for (size_t i = 0; i < values.size(); i++)
		values[i] = missingChecker().processMissingValue(_dataset->getFloatInfo(), values[i]);
	while (values.size() < numCases)
		values.push_back(NAN);
	// Have the columns do most of the work.
	column.setColumnAsScale(values);
}

/**
 * @brief setColumnConvrtStringData Sets String data into the column.
 * @param column The columns to insert into.
 */
void SPSSImportColumn::setColumnConvertStringData(Column &column)
{
	map<string, string> labels;
	CodePageConvert &strConvertor = _dataset->stringsConv();
	// Code page convert all strings.
	for (size_t i = 0; i < strings.size(); ++i)
		strings[i] = strConvertor.convertCodePage(strings[i]);

	for (SPSSImportColumn::LabelByValueDict::const_iterator it = spssLables.begin();
			it != spssLables.end(); ++it)
	{
		SpssDataCell cell = it->first;
		string value = string(cell.chars, sizeof(cell.chars));
		StrUtils::rTrimWSIP(value);
		labels[value] = it->second;
	}

	column.setColumnAsNominalString(strings, labels);
}

void SPSSImportColumn::setColumnConvertDblToString(Column &column)
{
	map<string, string> labels;
	strings.clear();
	for (size_t i = 0; i < numerics.size(); i++)
		strings.push_back( format(numerics[i], _dataset->getFloatInfo()));

	for (SPSSImportColumn::LabelByValueDict::const_iterator it = spssLables.begin();
			it != spssLables.end(); ++it)
	{
		labels[format(it->first.dbl, _dataset->getFloatInfo())] = it->second;
	}

	column.setColumnAsNominalString(strings, labels);
}

void SPSSImportColumn::setColumnAsNominalOrOrdinal(Column &column, Column::ColumnType columnType)
{
	size_t numCases = _dataset->numCases();

	// Add lables from the SPSS file first.
	map<int, string> labels;
	vector<int> dataToInsert;
	if (containsFraction())
	{
		setColumnConvertDblToString(column);
	}
	else
	{
		for (SPSSImportColumn::LabelByValueDict::const_iterator it = spssLables.begin();
				it != spssLables.end(); ++it)
			labels[static_cast<int>(it->first.dbl)] = it->second;

		// Add labels for numeric values (if not already present)..
		for (size_t i = 0; i < numCases; ++i)
		{
			if (missingChecker().isMissingValue(_dataset->getFloatInfo(), numerics[i]) == false)
			{
				int value = static_cast<int>(numerics[i]);
				dataToInsert.push_back(value);
				if (labels.find(value) == labels.end())
					labels.insert( pair<int, string>( value, format(numerics[i], _dataset->getFloatInfo() )));
			}
			else
				dataToInsert.push_back(INT_MIN);
		}

		column.setColumnAsNominalOrOrdinal(dataToInsert, labels, columnType);
	}
}

