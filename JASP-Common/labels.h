#ifndef LABELS_H
#define LABELS_H

#include "label.h"

#include <boost/container/vector.hpp>

typedef boost::interprocess::allocator<Label, boost::interprocess::managed_shared_memory::segment_manager> LabelAllocator;
typedef boost::container::vector<Label, LabelAllocator> LabelVector;

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

private:
	boost::interprocess::managed_shared_memory *_mem;
	LabelVector _labels;
};

#endif // LABELS_H
