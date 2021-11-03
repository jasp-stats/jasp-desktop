#ifndef READSTATIMPORTER_H
#define READSTATIMPORTER_H

#include "importer.h"
#include <string>
#include "column.h"

///
/// Uses ReadStat to import SPSS/SAS/STATA files and perhaps others.
class ReadStatImporter : public Importer
{

public:
	ReadStatImporter(std::string ext) : Importer(), _ext(ext)
	{
		DataSetPackage::pkg()->setIsArchive(false);

		if(_ext.size() == 0)	throw std::runtime_error("ReadStatImporter NEEDS to know the extension!");
		if(_ext[0] == '.')		_ext = _ext.substr(1);
	}
	~ReadStatImporter() override;

	static bool extSupported(const std::string & ext);
	void initColumn(QVariant colId, ImportColumn * importColumn) override;

protected:
	ImportDataSet *	loadFile(const std::string &locator, boost::function<void(int)> progressCallback)	override;

	std::string		_ext;

private:
	JASPTIMER_CLASS(ReadStatImporter);
};

#endif // READSTATIMPORTER_H
