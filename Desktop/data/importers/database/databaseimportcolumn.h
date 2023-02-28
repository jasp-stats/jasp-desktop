#ifndef CSVIMPORTCOLUMN_H
#define CSVIMPORTCOLUMN_H

#include "../importcolumn.h"
#include <QMetaType>
///
/// Storing a column during import from a database
class DatabaseImportColumn : public ImportColumn
{
public:
									DatabaseImportColumn(ImportDataSet* importDataSet, std::string name, QMetaType type);
									~DatabaseImportColumn()	override;

	size_t							size()									const	override;
	std::vector<std::string>		allValuesAsStrings()					const	override;
	void							addValue(const QVariant & value);
	const std::vector<QVariant> &	getValues()								const;
	QMetaType						type()									const { return _type; }


private:
	std::vector<QVariant>	_data;
	QMetaType				_type;

};

#endif // CSVIMPORTCOLUMN_H
