#ifndef DATASET_H
#define DATASET_H

#include "columns.h"

class DataSet
{
public:

	DataSet(boost::interprocess::managed_shared_memory *mem);
	~DataSet();

	int rowCount();
	int columnCount();

	Columns &columns();
	Column& column(int index);
	Column &column(std::string name);

	void setRowCount(int rowCount);
	void setColumnCount(int columnCount);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

private:

	Columns _columns;

	int _rowCount;
	int _columnCount;

};

#endif // DATASET_H
