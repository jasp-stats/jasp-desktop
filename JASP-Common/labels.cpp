#include "labels.h"

using namespace std;

Labels::Labels(boost::interprocess::managed_shared_memory *mem)
	: _labels(std::less<int>(), mem->get_segment_manager())
{
	_mem = mem;
}

void Labels::clear()
{
	_labels.clear();
}

int Labels::add(int display)
{
	return add(display, display);
}

int Labels::add(const std::string &display)
{
	return add(_labels.size(), display);
}

int Labels::add(int pos, int display)
{
	_labels[pos] = Label(display);

	return display;
}

int Labels::add(int pos, const std::string &display)
{
	_labels[pos] = Label(display, pos);

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

Labels::const_iterator Labels::begin() const
{
	return _labels.begin();
}

Labels::const_iterator Labels::end() const
{
	return _labels.end();
}
