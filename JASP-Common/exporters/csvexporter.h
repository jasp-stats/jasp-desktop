#ifndef CSVEXPORTER_H
#define CSVEXPORTER_H

#include <string>
#include "datasetpackage.h"
#include <boost/function.hpp>

class CSVExporter
{
public:
	static void saveDataSet(const std::string &path, DataSetPackage* package, boost::function<void (const std::string &, int)> progressCallback);
	static bool escapeValue(std::string &value);
};

#endif // CSVEXPORTER_H
