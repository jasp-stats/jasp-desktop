#ifndef DATASETLOADER_H
#define DATASETLOADER_H

#include "dataset.h"
#include "boost/signals2.hpp"

class DataSetLoader
{
public:
    DataSetLoader();
	DataSet *loadDataSet(const std::string &locator);
	void freeDataSet(DataSet *dataSet);

	boost::signals2::signal<void (const std::string &stage, int progress)> progress;

private:
	static void initColumn(Column &column, const std::string &name, const std::vector<std::string> &cells);
	static void growMemory(DataSet *&dataSet, boost::interprocess::managed_shared_memory *&mem);

};

#endif // DATASETLOADER_H
