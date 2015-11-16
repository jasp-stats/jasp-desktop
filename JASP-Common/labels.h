//
// Copyright (C) 2013-2015 University of Amsterdam
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

#ifndef LABELS_H
#define LABELS_H

#include "label.h"

#include <boost/container/map.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/segment_manager.hpp>

typedef std::pair<const int, Label> LabelEntry;
typedef boost::interprocess::allocator<LabelEntry, boost::interprocess::managed_shared_memory::segment_manager> LabelEntryAllocator;
typedef boost::container::map<int, Label, std::less<int>, LabelEntryAllocator> LabelMap;

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/const_iterator.hpp>

class Labels
{
public:
	Labels(boost::interprocess::managed_shared_memory *mem);

	void clear();
	int add(int display);
	int add(const std::string &display);
	int add(int raw, const std::string &display);
	int add(int raw, int display);

	const Label &at(int raw) const;
	size_t size() const;

	Labels& operator=(const Labels& labels);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	typedef LabelMap::const_iterator const_iterator;

	const_iterator begin() const;
	const_iterator end() const;

private:
	boost::interprocess::managed_shared_memory *_mem;
	LabelMap _labels;
};

namespace boost
{
	template <>
	struct range_const_iterator< Labels >
	{
		typedef Labels::const_iterator type;
	};
}


#endif // LABELS_H
