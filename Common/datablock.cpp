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

#include "datablock.h"

#include <algorithm>

DataBlock::DataBlock()
{
	_rowCount = 0;
}

bool DataBlock::insert(int rows)
{
	if (_rowCount + rows > BLOCK_SIZE)
		return false;

	_rowCount += rows;

	return true;
}

bool DataBlock::erase(int rows)
{
	if (_rowCount - rows < 0)
		return false;

	_rowCount -= rows;

	return true;
}

int DataBlock::rowCount()
{
	return _rowCount;
}

int DataBlock::capacity()
{
	return BLOCK_SIZE;
}
