#ifndef MINITABOLDIMPORTCOLUMN_H
#define MINITABOLDIMPORTCOLUMN_H

#include "../importcolumn.h"

///
/// Stores the relevant information of a column read from a Minitab file
/// (.mtw/.mpj/.mtp) through the mtw-ffi library. Numeric cells arrive as
/// doubles (NaN is a missing cell, Minitab's universal "*" sentinel), text
/// cells as strings (a blank "" is missing, just like in JASP itself).
class MinitabOldImportColumn : public ImportColumn
{
public:
							MinitabOldImportColumn(ImportDataSet * importDataSet, const std::string & name, columnType type);
							~MinitabOldImportColumn()	override;

			size_t			size()										const	override	{ return _values.size(); }
			columnType		getColumnType()								const	override	{ return _type; }
			const stringvec	allValuesAsStrings()						const	override	{ return _values; }
			std::string		valueLookup(size_t row)						const	override	{ return row < _values.size() ? _values[row] : ""; }
			const stringset	&	allEmptyValuesAsStrings()			const	override	{ return _missing; }

			void			addValue(double value);					///< NaN is stored as a missing value
			void			addValue(const std::string & value);	///< "" stays a blank, JASP treats it as missing
			void			reserve(size_t size)							{ _values.reserve(size); }

private:
	stringvec				_values;
	stringset				_missing;
	columnType				_type;
};

#endif // MINITABOLDIMPORTCOLUMN_H
