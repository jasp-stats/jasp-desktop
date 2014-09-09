#include "labels.h"

using namespace std;

Labels::Labels(boost::interprocess::managed_shared_memory *mem)
	: _labels(mem->get_segment_manager())
{
	_mem = mem;
}

void Labels::clear()
{
	_labels.clear();
}

int Labels::add(int display)
{
	int pos = _labels.size();
	_labels.push_back(Label(_mem, display));

	return pos;
}

int Labels::add(std::string &display)
{
	int pos = _labels.size();
	_labels.push_back(Label(_mem, display, _labels.size()));

	return pos;
}

const Label &Labels::at(int raw) const
{
	return _labels.at(raw);
}

size_t Labels::size() const
{
	return _labels.size();
}

Labels &Labels::operator=(const Labels &labels)
{
	if (&labels != this)
	{
		this->_mem = labels._mem;
		this->_labels = labels._labels;
	}

	return *this;
}

void Labels::setSharedMemory(boost::interprocess::managed_shared_memory *mem)
{
	_mem = mem;
}
