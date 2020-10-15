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

#ifndef LABEL_H
#define LABEL_H

#include <string>

/*********
 * Label is a class that stores the value of a column if it is not a Scale (a Nominal Int, Nominal Text, or Ordinal).
 * The value is either an integer or a string.
 * If it is an integer, the _intValue is this value, and _stringValue is at first the corresponding string.
 * _stringValue can be then changed in the Variable tab in JASP.
 * If the value is a string, _intValue is the key that maps the label with the AsInts property of the column object.
 * _stringValue is then the value, that can be changed in the Variable tab in JASP. If changed the original value
 * is saved in the _orgStringValues static property of the Labels class.
 *********/

class Label
{
public:
	static const int MAX_LABEL_LENGTH = 128;
	Label(const std::string &label, int value, bool filterAllows, bool isText = true);
	Label(int value);
	Label();

	std::string text() const;
	bool hasIntValue() const;
	int value() const;
    void setLabel(const std::string &label);
	void setValue(int value);
	Label& operator=(const Label &label);

	bool filterAllows() const { return _filterAllow; }
	void setFilterAllows(bool allowFilter) { _filterAllow = allowFilter; }

private:

	bool _hasIntValue;
	int _intValue;
	char _stringValue[MAX_LABEL_LENGTH];
	int  _stringLength;

    void _setLabel(const std::string &label);

	bool _filterAllow = true;
};

#endif // LABEL_H
