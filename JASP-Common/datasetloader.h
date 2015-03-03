#ifndef DATASETLOADER_H
#define DATASETLOADER_H

#include "dataset.h"
#include "boost/function.hpp"

class DataSetLoader
{
public:

	static DataSet *loadDataSet(const std::string &locator, boost::function<void (const std::string &stage, int progress)> progressCallback = NULL);
	static void freeDataSet(DataSet *dataSet);

	static DataSet *getDataSet();

private:

	static std::string deEuropeanise(const std::string &value);

	static void initColumn(Column &column, const std::string &name, const std::vector<std::string> &cells);
	static void growMemory(DataSet *&dataSet, boost::interprocess::managed_shared_memory *&mem);

};

#endif // DATASETLOADER_H
