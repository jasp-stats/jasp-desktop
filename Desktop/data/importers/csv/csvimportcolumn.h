#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "../importcolumn.h"

///
/// Storing a column during import of a CSV
class CSVImportColumn : public ImportColumn
{
public:
									CSVImportColumn(ImportDataSet* importDataSet, std::string name);
									~CSVImportColumn()	override;

	size_t							size()									const	override;
	std::vector<std::string>		allValuesAsStrings()					const	override { return  _data; }
	void							addValue(const std::string &value);
	const std::vector<std::string>& getValues()								const;


private:
	std::vector<std::string> _data;

};

#endif // CSVIMPORTCOLUMN_H
