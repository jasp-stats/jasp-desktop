//
// Copyright (C) 2013-2021 University of Amsterdam
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
#include "version.h"
#include <regex>
#include <sstream>

const char * Version::encodingError::what() const noexcept
{
	//Just here to have an out-of-line virtual method so that clang and gcc don't complain so much
	return std::runtime_error::what();
}


Version::Version(std::string version)
{
	const static std::regex parseIt("(\\d+)(\\.(\\d+))?(\\.(\\d+))?(\\.(\\d+))?"); //(sub)groups: 1 3 5 7//0 is whole match/line

	if (version.empty()) return; // Empty version.

	std::smatch found;
	if(!std::regex_match(version, found, parseIt))
		throw encodingError(version);

	std::string majorStr	= found[1],
				minorStr	= found[3],
				releaseStr	= found[5],
				fourthStr	= found[7];


	try
	{
		if(majorStr		!= "")	_major		= std::stoul(majorStr);
		if(minorStr		!= "")	_minor		= std::stoul(minorStr);
		if(releaseStr	!= "")	_release	= std::stoul(releaseStr);
		if(fourthStr	!= "")	_fourth		= std::stoul(fourthStr);
	}
	catch(...) { throw encodingError(version); }
}

std::string Version::asString(size_t nums) const
{
	bool	addFourth	=				_fourth  > 0 || nums > 3,
			addRelease	= addFourth  || _release > 0 || nums > 2,
			addMinor	= addRelease || _minor   > 0 || nums > 1;

	std::stringstream out;
						out			<< std::to_string(_major);
	if(addMinor)		out << "."	<< std::to_string(_minor);
	if(addRelease)		out << "."	<< std::to_string(_release);
	if(addFourth)		out << "."	<< std::to_string(_fourth);

	return out.str();
}

void Version::swap(Version &other)
{
	std::swap(_major,	other._major	);
	std::swap(_minor,	other._minor	);
	std::swap(_release, other._release	);
	std::swap(_fourth,  other._fourth	);
}

bool	Version::operator ==	(const Version & other) const {	return !operator!=(other);						}
bool	Version::operator <=	(const Version & other) const {	return operator==(other) || operator<(other);	}
bool	Version::operator >=	(const Version & other) const {	return !operator<(other);						}
bool	Version::operator >		(const Version & other) const { return operator!=(other) && operator>=(other);	}

bool Version::operator !=	(const Version & other) const
{
	return _major != other._major || _minor != other._minor || _release != other._release || _fourth != other._fourth;
}

bool Version::operator <	(const Version & other) const
{
	if(_major	< other._major)		return true;
	if(_major	> other._major)		return false;

	if(_minor	< other._minor)		return true;
	if(_minor	> other._minor)		return false;

	if(_release < other._release)	return true;
	if(_release > other._release)	return false;

	if(_fourth	< other._fourth)	return true;

	return false;
}


bool Version::isEmpty() const
{
	return
		_major		== 0 &&
		_minor		== 0 &&
		_release	== 0 &&
		_fourth		== 0;
}
