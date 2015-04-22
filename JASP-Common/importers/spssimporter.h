#ifndef SPSSIMPORTER_H
#define SPSSIMPORTER_H

#include "datasetpackage.h"
#include <boost/function.hpp>
#include <string>

class SPSSImporter
{
public:

	static void loadDataSet(DataSetPackage *packageData, const std::string &locator, boost::function<void (const std::string &, int)> progress);
};

#endif // SPSSIMPORTER_H
