#ifndef READSTATIMPORTER_H
#define READSTATIMPORTER_H

#include "importer.h"
#include <string>
#include "column.h"


class ReadStatImporter : public Importer
{

public:
	ReadStatImporter(DataSetPackage *packageData, std::string ext) : Importer(packageData), _ext(ext)
	{
		_packageData->setIsArchive(false);

		if(_ext.size() == 0)	throw std::runtime_error("ReadStatImporter NEEDS to know the extension!");
		if(_ext[0] == '.')		_ext = _ext.substr(1);
	}
	~ReadStatImporter() override;

	static bool extSupported(const std::string & ext);
	void initColumn(QVariant colId, ImportColumn * importColumn) override;

protected:
	ImportDataSet *	loadFile(const std::string &locator, boost::function<void(const std::string &, int)> progressCallback)	override;

	std::string		_ext;
};

#endif // READSTATIMPORTER_H
