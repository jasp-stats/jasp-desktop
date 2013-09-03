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

	boost::signals2::signal<void (std::string stage, int progress)> progress;

};

#endif // DATASETLOADER_H
