
#include "label.h"

#include <sstream>
#include <cstring>

Label::Label(const std::string &label, int value)
{
	_stringLength = label.length();
	if (sizeof(_stringValue) < label.length())
		_stringLength = sizeof(_stringValue);

	std::memcpy(_stringValue, label.c_str(), _stringLength);

	_hasIntValue = true;
	_intValue = value;
}

Label::Label(int value)
{
	std::stringstream ss;
	ss << value;
	std::string asString = ss.str();

	std::memcpy(_stringValue, asString.c_str(), asString.length());
	_stringLength = asString.length();

	_hasIntValue = true;
	_intValue = value;
}

Label::Label()
{
	_hasIntValue = false;
	_intValue = -1;
	_stringLength = 0;
}

std::string Label::text() const
{
	return std::string(_stringValue, _stringLength);
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

	std::memcpy(_stringValue, label._stringValue, label._stringLength);
	_stringLength = label._stringLength;

	return *this;
}
