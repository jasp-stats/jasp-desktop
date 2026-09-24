#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "../importcolumn.h"
#include "columnutils.h"

///
/// Storing a column during import of a CSV
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
	stringvec				_data;
	ColumnUtils::toDoubleF	_readNumbersAs;	///< Reads numbers in the locale chosen for this file in the csv preview, empty means that of the interface

};

#endif // CSVIMPORTCOLUMN_H
