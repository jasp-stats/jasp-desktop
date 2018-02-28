//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef DATABLOCK_H
#define DATABLOCK_H

#define BLOCK_SIZE 512

class DataBlock
{

public:
	DataBlock();

	bool insert(int rows);
	bool erase(int rows);

	int _rowCount;

	union { double d; int i; } Data[BLOCK_SIZE];

	int rowCount();
	static int capacity();
};

#endif // DATABLOCK_H
