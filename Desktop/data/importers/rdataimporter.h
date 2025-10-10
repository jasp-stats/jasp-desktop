#ifndef RDATAIMPORTER_H
#define RDATAIMPORTER_H

#include "importer.h"
#include <QCoreApplication>
#include "timers.h"


class RDataImporter : public Importer
{

public:
	RDataImporter() : Importer()
	{
	}
	~RDataImporter() override;
	
protected:
	ImportDataSet* loadFile(const std::string &locator, std::function<void(int)> progressCallback) override;
	
private:
	JASPTIMER_CLASS(RDataImporter);
};

#endif // RDATAIMPORTER_H
