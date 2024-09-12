#ifndef EXCELIMPORTCOLUMN_H
#define EXCELIMPORTCOLUMN_H

#include "data/importers/importcolumn.h"


class ExcelImportColumn : public ImportColumn
{
public:
	ExcelImportColumn(ImportDataSet* importDataSet, std::string name);
	ExcelImportColumn(ImportDataSet* importDataSet, std::string name, long reserve);
	~ExcelImportColumn()	override;

	size_t	size()	const	override;
	const	stringvec	&	allValuesAsStrings()    const	override { return  _data; }
	void					addValue(const std::string &value);
	const	stringvec	&	getValues()     const;


private:
	stringvec _data;

};


#endif // EXCELIMPORTCOLUMN_H
