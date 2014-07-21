
#include "label.h"

using boost::interprocess::managed_shared_memory;

Label::Label(managed_shared_memory *mem, const std::string &label, int value)
	: _stringValue(label.begin(), label.end(), mem->get_segment_manager())
{
	_mem = mem;
	_hasIntValue = true;
	_intValue = value;
}

Label::Label(managed_shared_memory *mem, int value)
	: _stringValue(mem->get_segment_manager())
{
	_mem = mem;

	std::stringstream ss;
	ss << value;
	std::string asString = ss.str();

	_stringValue = String(asString.begin(), asString.end(), _mem->get_segment_manager());
	_hasIntValue = true;
	_intValue = value;
}

std::string Label::text() const
{
	return std::string(_stringValue.begin(), _stringValue.end());
}

bool Label::hasIntValue() const
{
	return _hasIntValue;
}

int Label::value() const
{
	return _intValue;
}

Label &Label::operator=(const Label &label)
{
	this->_hasIntValue = label._hasIntValue;
	this->_intValue = label._intValue;
	this->_mem = label._mem;
	this->_stringValue = label._stringValue;

	return *this;
}
