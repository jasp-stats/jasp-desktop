#ifndef DATASET_H
#define DATASET_H

#include "columns.h"

class DataSet
{
public:

	DataSet();
	~DataSet();

	int rowCount();
	int columnCount();

	Columns &columns();

	void setRowCount(int rowCount);
	void setColumnCount(int columnCount);

private:

	Columns _columns;

	int _rowCount;
	int _columnCount;

};

#endif // DATASET_H
