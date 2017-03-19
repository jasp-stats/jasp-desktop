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

#ifndef LABEL_H
#define LABEL_H

#include <string>

class Label
{
public:
	Label(const std::string &label, int value);
	Label(int value);
	Label();

	std::string text() const;
	bool hasIntValue() const;
	int value() const;
    void setLabel(const std::string &label);
	Label& operator=(const Label &label);

private:

	bool _hasIntValue;
	int _intValue;
	char _stringValue[128];
	int  _stringLength;

    void _setLabel(const std::string &label);
};

#endif // LABEL_H
