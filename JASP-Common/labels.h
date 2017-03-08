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

#ifndef LABELS_H
#define LABELS_H

#include "label.h"
#include <map>
#include <vector>
#include <set>

#include <boost/container/vector.hpp>
#include <boost/container/map.hpp>

#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/segment_manager.hpp>

typedef boost::interprocess::allocator<Label, boost::interprocess::managed_shared_memory::segment_manager> LabelAllocator;
typedef boost::container::vector<Label, LabelAllocator> LabelVector;

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/const_iterator.hpp>

class Labels
{
public:
	Labels(boost::interprocess::managed_shared_memory *mem);
	virtual ~Labels();

	void clear();
	int add(int display);
	int add(const std::string &display);
	int add(int index, const std::string &display);
	void syncInts(const std::set<int> &values);
	void syncInts(std::map<int, std::string> &values);
	std::map<std::string, int> syncStrings(const std::vector<std::string> &new_values, const std::map<std::string, std::string> &new_labels);

	const Label &labelFor(int index) const;
	void set(std::vector<Label> &labels);
	size_t size() const;

	Labels& operator=(const Labels& labels);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);
	typedef LabelVector::const_iterator const_iterator;

	const_iterator begin() const;
	const_iterator end() const;

	std::map<int, std::string> &getOrgStringValues();
	void setOrgStringValues(int key, std::string value);

	bool setLabelFromRow(int row, const std::string &display);
	void setLabelFromValue(int value, const std::string &display);
	std::string getLabelFromRow(int);
	std::string getValueFromRow(int);
	std::string getValueFromIndex(int);

private:
	void _setNewStringForLabel(Label &label, const std::string &display);
	std::string _getValueFromLabel(const Label &label);

	boost::interprocess::managed_shared_memory *_mem;
	LabelVector _labels;
	int _id;
	static int _counter;
	// Original string values: used only when value is a string and when the label has been changed
	// This map is not in the shared memory (it's only used by the JASP-Desktop): this allows this map to grow
	// without risking to fill up the shared memory.
	static std::map<int, std::map<int, std::string> > _orgStringValues;
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
