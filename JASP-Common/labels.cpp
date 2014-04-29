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
	if (_labels.size() > 0 && _haveIntegerValues == false)
		throw exception();

	_haveIntegerValues = true;
	int pos = _labels.size();
	_labels.push_back(Label(_mem, display));

	return pos;
}

int Labels::add(std::string &display)
{
	if (_labels.size() > 0 && _haveIntegerValues)
		throw exception();

	_haveIntegerValues = false;
	int pos = _labels.size();
	_labels.push_back(Label(_mem, display));

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

bool Labels::haveIntegerValues() const
{
	return _haveIntegerValues;
}

Labels &Labels::operator=(const Labels &labels)
{
	if (&labels != this)
	{
		this->_mem = labels._mem;
		this->_labels = labels._labels;
		this->_haveIntegerValues = labels._haveIntegerValues;
	}

	return *this;
}
