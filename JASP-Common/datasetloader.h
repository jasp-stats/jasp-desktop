#ifndef DATASETLOADER_H
#define DATASETLOADER_H

#include "dataset.h"
#include <boost/function.hpp>

class DataSetLoader
{
public:

	static DataSet *loadDataSet(const std::string &locator, boost::function<void (const std::string &stage, int progress)> progress = NULL);
	static DataSet *getDataSet();
	static void freeDataSet(DataSet *dataSet);

};

#endif // DATASETLOADER_H
