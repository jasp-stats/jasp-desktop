#ifndef RDATAIMPORTER_H
#define RDATAIMPORTER_H

#include "importer.h"
#include <QCoreApplication>
#include "timers.h"


class RDataImporter : public Importer
{

public:
	RDataImporter(std::string ext) : Importer(), _ext(stringUtils::toLower(ext))
	{
		if(_ext.size() == 0)	throw std::runtime_error("Rdata reader NEEDS to know the extension!");
		if(_ext[0] == '.')		_ext = _ext.substr(1);
	}
	~RDataImporter() override;
	
protected:
	ImportDataSet* loadFile(const std::string &locator, std::function<void(int)> progressCallback) override;
	std::string		_ext;
	
private:
	JASPTIMER_CLASS(RDataImporter);
};

#endif // RDATAIMPORTER_H
