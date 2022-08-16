#ifndef DATABASEIMPORTER_H
#define DATABASEIMPORTER_H

#include "importer.h"
#include "data/databaseconnectioninfo.h"

class DatabaseImporter : public Importer
{
	Q_DECLARE_TR_FUNCTIONS(DatabaseImporter)

	typedef DatabaseConnectionInfo Info;
	
public:
	DatabaseImporter(){}
	
	ImportDataSet* loadFile(const std::string &locator, boost::function<void(int)> progressCallback) override;
	void initColumn(QVariant colId, ImportColumn * importColumn) override;
	
	DatabaseConnectionInfo _info;
};

#endif // DATABASEIMPORTER_H
