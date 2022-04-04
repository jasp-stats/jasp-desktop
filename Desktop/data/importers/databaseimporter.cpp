#include "databaseimporter.h"

ImportDataSet * DatabaseImporter::loadFile(const std::string &locator, boost::function<void(int)> progressCallback)
{
	// locator is the result of DatabaseConnectionInfo::toJson, so:
	Json::Value json;
	if(!Json::Reader().parse(locator, json))
		throw std::runtime_error("DatabaseImporter::loadFile received illegal locator!"); //shouldnt occur normally
	
	_info = DatabaseConnectionInfo(json);
	
	if(!_info.connect())
		throw std::runtime_error(fq(tr("Failed to connect to database %1 at %2 with user %3, last error was: '%4'")
										.arg(_info._database)
										.arg(_info._hostname + ":" + tq(std::to_string(_info._port)))
										.arg(_info._username)
										.arg(_info.lastError()));
	
	
}
