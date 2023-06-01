#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "../importcolumn.h"

///
/// Storing a column during import of a CSV
class CSVImportColumn : public ImportColumn
{
public:
							CSVImportColumn(ImportDataSet* importDataSet, std::string name);
							CSVImportColumn(ImportDataSet* importDataSet, std::string name, long reserve);
							~CSVImportColumn()	override;

			size_t			size()									const	override;
	const	stringvec	&	allValuesAsStrings()					const	override { return  _data; }
			void			addValue(const std::string &value);
	const	stringvec	&	getValues()								const;


private:
	stringvec _data;

};

#endif // CSVIMPORTCOLUMN_H
