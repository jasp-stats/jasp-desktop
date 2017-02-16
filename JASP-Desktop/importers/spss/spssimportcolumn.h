#ifndef SPSSIMPORTCOLUMN_H
#define SPSSIMPORTCOLUMN_H

#include "../importerutils.h"
#include "../convertedstringcontainer.h"
#include "../importcolumn.h"
#include "spssformattype.h"
#include "missingvaluechecker.h"
#include "measures.h"
#include "systemfileformat.h"

#include <qdatetime.h>


namespace spss
{
class SPSSImportDataSet;

class SPSSImportColumn : public ConvertedStringContainer, public ImportColumn
{
public:
	typedef std::map<SpssDataCell, std::string>		LabelByValueDict;
	typedef std::pair<SpssDataCell, std::string>	LabelByValueDictEntry;


	/**
	 * @brief SPSSColumn Ctor
	 * @param dataset SPSS DataSet
	 * @param name SPSS column name (short form)
	 * @param label Column label.
	 * @param stringLen String length or zero.
	 * @param formattype The format Type from the variable record.
	 * @param missingChecker Check for missing value with this.
	 */
	SPSSImportColumn(SPSSImportDataSet *spssdataset, const std::string &name, const std::string &label, long stringLen, FormatTypes formattype, const spss::MissingValueChecker &missingChecker);

	~SPSSImportColumn();

	/*
	 * Attributes for the column.
	 */
	WRITE_ATTR(std::string, spssLongColName) // The name as shown to the user.
	WRITE_ATTR(std::string, spssColumnLabel) // The name as shown to the user.

	READ_ATTR(std::string, spssRawColName)	// The name as in the file.

	READ_ATTR(size_t, spssStringLen)		// Length of the string (if string).

	READ_ATTR(size_t, columnSpan)			// Number of data cells this column spans.
	void incrementColumnSpan() { _columnSpan++; }

	RW_ATTR(Measure, spssMeasure)			// The Measure from the SPSS file (if any)
	READ_ATTR(FormatTypes, spssFormatType)	// The Format / Type value from the SPSS file.

	READ_ATTR(spss::MissingValueChecker, missingChecker) // A Missing value checker machine.

	RW_ATTR(size_t, charsRemaining)		// The numer of chars (in this case!) remaind to read-in.

	std::vector<double>			numerics;	// Numeric values, one per case.
	std::vector<std::string>	strings;	// String values, one per case.
	LabelByValueDict			spssLables;	// Lables as found in the .SAV

	enum e_celTypeReturn { cellDouble, cellString };
	/**
	 * @brief cellType Gets the cell data type we expect to read for this coloumn.
	 * @return
	 */
	enum e_celTypeReturn cellType() const;

	virtual size_t size() const;
	virtual bool isValueEqual(Column &col, size_t row) const;

	const std::string& setSuitableName();

	/**
	 * @brief insert Insert a string into the columns.
	 * @param str String to insert.
	 * @return index of the inserted value (=== case)
	 */
	size_t insert(const std::string &str);

	/**
	 * @brief append Appends a value to the last inserted string.
	 * @param str The string to append.
	 * @return index of the inserted value (=== case)
	 */
	size_t append(const std::string &str);

	/**
	 * @brief insert Inserts a double.
	 * @param value The value to insert,
	 * @return index of the inserted value (=== case)
	 */
	size_t insert(double value);


	/**
	 * @brief spssStringLen Set the length of the string (in column).
	 * @param value Value to set.
	 */
	void spssStringLen(size_t value);

	/**
	 * @brief charsRemaining Find the number of chars remaining for this buffer, for one data cell.
	 * @param bufferSzie The size of the buffer.
	 * @return Value
	 */
	long cellCharsRemaining(size_t bufferSize);

	/**
	 * @brief getJaspColumnType Finds the column type that JASP will use.
	 * @return A column type.
	 *
	 */
	Column::ColumnType getJaspColumnType() const;

	/**
	 * @brief format Fomats a number that SPSS holds as a numeric value that JASP cannot deal with.
	 * @param value The value to format.
	 * @param floatInfo The float info record we have.
	 * @return
	 */
	std::string format(double value, const FloatInfoRecord &floatInfo) const;

	/**
	 * @brief containsFraction Returns false if all values are integer.
	 * @param values VAlues to check
	 * @return true if a fractional part found.
	 */
	bool containsFraction() const { return _containsFraction(numerics); }

	/**
	 * @brief setColumnConvertStringData Sets String data into the column, after doing a code page convert.
	 * @param column The columns to insert into.
	 */
	void setColumnConvertStringData(Column &column);

	/**
	 * @brief setColumnConvertDblToString Sets String data into the column.
	 * @param column The columns to insert into.
	 */
	void setColumnConvertDblToString(Column &column);

	/**
	 * @brief setColumnAsNominalOrOrdinal Sets numeric data into the column, with labels.
	 * @param column The columns to insert into.
	 */
	void setColumnAsNominalOrOrdinal(Column &column, Column::ColumnType columnType);

	/**
	 * @brief setColumnScaleData Sets floating point / scalar data into the column.
	 * @param column The columns to insert into.
	 */
	void setColumnScaleData(Column &column);

	protected:

	SPSSImportDataSet* _dataset;

	/**
	 * @brief processStrings Converts any strings in the data fields.
	 * @param dictData The
	 *
	 * Should be implemented in classes where holdStrings maybe or is true.
	 *
	 */
	virtual void processStrings(const CodePageConvert &converter);

	private:
	// Day Zero for spss files.
	static const QDate _beginEpoch;

	/**
	 * @brief _toQDateTime Convert SPSS seconds to a date/time.
	 * @param dt - Target
	 * @param seconds Number of seconds since start of SPSS epoch.
	 * @return void
	 */
	static QDateTime* _asDateTime(double seconds);

	/**
	 * @brief containsFraction Returns false if all values are integer.
	 * @param values VAlues to check
	 * @return true if a fractional part found.
	 */
	static bool _containsFraction(const std::vector<double> &values);

	/*
	 * Help functions for _toDateTime().
	 */
	QString _weekday(unsigned short int wd) const;

	QString _month(unsigned short int mnth) const;
};

} // end namespace
#endif // SPSSIMPORTCOLUMN_H
