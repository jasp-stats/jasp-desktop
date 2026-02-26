#ifndef MINITABIMPORTCOLUMN_H
#define MINITABIMPORTCOLUMN_H


#include "data/importers/importcolumn.h"


class MwxImportColumn : public ImportColumn
{
    Q_OBJECT
public:
		MwxImportColumn(ImportDataSet *importDataSet, const std::string & name, const stringvec & _levels, columnType type);
		~MwxImportColumn() override;


		size_t							size()											const override;
		const stringvec			allValuesAsStrings()				const override { return _data; }
		std::string					valueLookup(size_t row)			const override;
	
		std::string					labelLookup(size_t row)			const override;
		columnType					type()											const  { return _type; }
		
		void								addValue(const std::string &value, int index = -1);
		void								reserve(size_t size);

private:
    stringvec					_data,
											_levels;
		columnType				_type;
};

#endif // MINITABIMPORTCOLUMN_H
