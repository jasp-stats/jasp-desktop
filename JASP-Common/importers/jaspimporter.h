#ifndef JASPIMPORTER_H
#define JASPIMPORTER_H


#include "datasetpackage.h"

#include <boost/function.hpp>

#include <string>
#include <vector>

class JASPImporter
{
public:
	static void loadDataSet(DataSetPackage *packageData, const std::string &path, boost::function<void (const std::string &, int)> progressCallback);

private:
	static Column::ColumnType getColumnType(std::string name);
	static bool parseJsonEntry(Json::Value &root, const std::string &path, const std::string &entry, bool required);
};

#endif // JASPIMPORTER_H
