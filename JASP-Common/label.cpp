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

#include "label.h"

#include <sstream>
#include <cstring>

void Label::_setLabel(const std::string &label) {
    _stringLength = label.length();
    if (sizeof(_stringValue) < label.length())
        _stringLength = sizeof(_stringValue);

    std::memcpy(_stringValue, label.c_str(), _stringLength);
}

Label::Label(const std::string &label, int value, bool filterAllows, bool isText)
{
    _setLabel(label);
	_hasIntValue = !isText;
	_intValue = value;
	_filterAllow = filterAllows;
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

void Label::setLabel(const std::string &label) {
	_setLabel(label);
}

void Label::setValue(int value)
{
	_intValue = value;
}

Label &Label::operator=(const Label &label)
{
	this->_hasIntValue	= label._hasIntValue;
	this->_intValue		= label._intValue;
	this->_filterAllow	= label._filterAllow;

	std::memcpy(_stringValue, label._stringValue, label._stringLength);
	_stringLength = label._stringLength;

	return *this;
}
