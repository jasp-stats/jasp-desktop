#ifndef SPSSIMPORTER_H
#define SPSSIMPORTER_H

#include "dataset.h"
#include <boost/function.hpp>
#include <string>

class SPSSImporter
{
public:

	static DataSet *loadDataSet(const std::string &locator, boost::function<void (const std::string &, int)> progress);
};

#endif // SPSSIMPORTER_H
