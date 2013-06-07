#include "datablock.h"

#include <algorithm>

DataBlock::DataBlock()
{
	_rowCount = 0;
}

bool DataBlock::insert(int position, int rows)
{
	if (_rowCount + rows > BLOCK_SIZE)
		return false;

	_rowCount += rows;

	for (int i = _rowCount - 1; i >= position + rows; i--)
		Data[i].d = Data[i - rows].d;

}

int DataBlock::rowCount()
{
	return _rowCount;
}

int DataBlock::capacity()
{
	return BLOCK_SIZE;
}

void DataBlock::moveTo(DataBlock *dest, int position, int rows)
{
	if (dest->insert(position, rows) == false)
	{
        //qDebug() << "could not move!";
		return;
	}

}

