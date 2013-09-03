#ifndef DATABLOCK_H
#define DATABLOCK_H

#define BLOCK_SIZE 32767

class DataBlock
{

public:
	DataBlock();

	bool insert(int position, int rows);

	int _rowCount;

	union { double d; int i; } Data[BLOCK_SIZE];

	int rowCount();
	static int capacity();

	double take(int position);

	void moveTo(DataBlock *dest, int position, int rows);
};

#endif // DATABLOCK_H
