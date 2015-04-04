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
	int add(std::string &display);

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
