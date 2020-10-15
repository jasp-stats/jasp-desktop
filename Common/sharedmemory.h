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

#ifndef SHAREDMEMORY_H
#define SHAREDMEMORY_H

#include <boost/interprocess/managed_shared_memory.hpp>
#include "dataset.h"

/*
 * Data sets are created in shared memory, allowing the UI process
 * and the background processes to all access to it.
 * The UI process calls createDataSet(), and the background
 * processes latch onto this with retrieveDataSet()
 * A DataSet needs to be able to allocate memory, so the memory
 * segment is passed into the DataSet's constructor.
 * This allows the DataSet to allocate child objects (such as columns)
 * in shared memory as well.
 * Good examples of creating and populating a DataSet can be found
 * in the importers
 */

class SharedMemory
{
public:

	static DataSet	*createDataSet();
	static DataSet	*retrieveDataSet(unsigned long parentPID = 0);
	static DataSet	*enlargeDataSet(DataSet *dataSet);
	static void		deleteDataSet(DataSet *dataSet);
	static void		unloadDataSet();
private:

	static std::string _memoryName;
	static boost::interprocess::managed_shared_memory *_memory;

};

#endif // SHAREDMEMORY_H
