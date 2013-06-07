#ifndef DATASETLOADER_H
#define DATASETLOADER_H

#include "dataset.h"

using namespace std;

class DataSetLoader
{
public:
    DataSetLoader();
    static DataSet *loadFile(istream &is);
};

#endif // DATASETLOADER_H
