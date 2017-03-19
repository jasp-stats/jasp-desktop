//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef DATASET_H
#define DATASET_H

#include "columns.h"

class DataSet
{
public:

	DataSet(boost::interprocess::managed_shared_memory *mem);
	~DataSet();

	int rowCount() const;
	int columnCount() const;

	Columns& columns();
	Column& column(int index);
	Column& column(std::string name);

	int getColumnIndex(std::string name);
	void setRowCount(int rowCount);
	void setColumnCount(int columnCount);
	void removeColumn(std::string name);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	std::string toString();

private:

	Columns _columns;

	int _rowCount;
	int _columnCount;

};

#endif // DATASET_H
