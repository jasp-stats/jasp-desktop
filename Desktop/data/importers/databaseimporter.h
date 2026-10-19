#ifndef DATABASEIMPORTER_H
#define DATABASEIMPORTER_H

#include "importer.h"
#include "databaseconnectioninfo.h"

class DatabaseImporter : public Importer
{
	Q_DECLARE_TR_FUNCTIONS(DatabaseImporter)

	typedef DatabaseConnectionInfo Info;
	
public:
	DatabaseImporter() : Importer() {}
	
	ImportDataSet* loadFile(const std::string &locator, std::function<void(int)> progressCallback) override;
	
	DatabaseConnectionInfo * _info = nullptr;
};

#endif // DATABASEIMPORTER_H
