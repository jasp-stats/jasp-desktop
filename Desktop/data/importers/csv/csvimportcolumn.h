#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "../importcolumn.h"
#include "columnutils.h"

///
/// Storing a column during import of a CSV.
/// When the csv preview picked the locale the numbers of the file are written in (readNumbersAs, see CsvPreviewModel), a value is read
/// in that locale or else the way C writes numbers, but never in the locale of the interface: text that locale does not take for a number
/// stays text. valueLookup then hands a number over written the way C writes it, which the column reads without any locale (valuesUseLocale),
/// and valueLookupAsShown writes it the way the column shows it once imported, for a sync to compare with.
class CSVImportColumn : public ImportColumn
{
public:
							CSVImportColumn(ImportDataSet* importDataSet, std::string name);
							CSVImportColumn(ImportDataSet* importDataSet, std::string name, long reserve, ColumnUtils::toDoubleF readNumbersAs = nullptr);
							~CSVImportColumn()	override;

			size_t			size()									const	override;
	const	stringvec		allValuesAsStrings()					const	override	{ return _data; }
	std::string				valueLookup(size_t row)					const	override;
			bool			valuesUseLocale()						const	override	{ return !_readNumbersAs; }
	std::string				valueLookupAsShown(size_t row)			const	override;
			void			addValue(const std::string &value);
	const	stringvec	&	getValues()								const;


private:
	bool					_readNumber(const std::string & text, double & number) const;

	stringvec				_data;
	ColumnUtils::toDoubleF	_readNumbersAs;	///< Reads numbers in the locale picked for this file in the csv preview, empty when none was picked

};

#endif // CSVIMPORTCOLUMN_H
