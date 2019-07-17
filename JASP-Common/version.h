//
// Copyright (C) 2018 University of Amsterdam
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

#ifndef VERSION_H
#define VERSION_H

#include <string>

class Version
{
public:
	Version(unsigned char _major, unsigned char _minor, unsigned char _revision, unsigned short _build);
	Version(std::string version);
	Version() {}

	bool operator<(const Version&)	const;
	bool operator>(const Version&)	const;
	bool operator<=(const Version&) const;
	bool operator>=(const Version&) const;
	bool operator==(const Version&) const;
	bool operator!=(const Version&) const;

	bool isRelease()	const;
	bool isAlpha()		const;
	bool isBeta()		const;
	bool isEmpty()		const;

	std::string asString(bool addDebugFlag = false) const;

	unsigned char	major		= 0,
					minor		= 0,
					revision	= 0,
					build		= 0;
};


#endif // VERSION_H
