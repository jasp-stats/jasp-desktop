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


Version::Version(const char * version)
{
	fromString(version);
}

Version::Version(const std::string & version)
{
	fromString(version);
}

void Version::fromString(const std::string & version)
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


void split_helper(const std::string& in, const char delim, std::vector<std::string>& parts) {
	std::stringstream ss(in);
	std::string segment;

	parts.clear();
	while(std::getline(ss, segment, delim)) {
		parts.push_back(segment);
	}
}

BundleVersion::BundleVersion(const std::string &version)
{
	std::string normalVersionNums;
	std::string extension = "";

	std::vector<std::string> split;
	split_helper(version, '-', split);
	if(split.size() == 1) normalVersionNums = version;
	else if(split.size() == 2) {
		normalVersionNums = split[0];
		extension = split[1];
	}
	else throw Version::encodingError("multiple '-' in Version number: " + version);

	fromString(normalVersionNums);
	if(!extension.empty()) {
		//parse extension
		split_helper(extension, '.', split);
		if(split.size() == 1) {
			_type = typefromString(extension);
		}
		else if(split.size() == 2) {
			_type = typefromString(split[0]);
			_buildnum = std::stoul(split[1]);
		}
		else throw Version::encodingError("corrupt version extension in: " + version);
	}

}

std::string BundleVersion::asString(size_t versionNumbersToInclude) const
{
	return Version::asString(versionNumbersToInclude) + "-" + BundleVersion::typetoString(_type) + "." + std::to_string(_buildnum);
}

bool BundleVersion::operator <(const BundleVersion &other) const
{
	if(Version::operator!=(other))
		return Version::operator<(other);
	if(_type != other._type)
		return _type < other._type;
	else
		return _buildnum < other._buildnum;

}

bool BundleVersion::operator !=(const BundleVersion &other) const
{
	return Version::operator!=(other) || _buildnum != other._buildnum || _type != other._type;
}

bool	BundleVersion::operator ==	(const BundleVersion & other) const {	return !operator!=(other);						}
bool	BundleVersion::operator <=	(const BundleVersion & other) const {	return operator==(other) || operator<(other);	}
bool	BundleVersion::operator >=	(const BundleVersion & other) const {	return !operator<(other);						}
bool	BundleVersion::operator >	(const BundleVersion & other) const { return operator!=(other) && operator>=(other);	}

std::string BundleVersion::typetoString(const Type type) const
{
	switch (type) {
	case Type::Alpha:
		return "alpha";
	case Type::Beta:
		return "beta";
	case Type::Release:
		return "release";
    default:
        throw Version::encodingError("Unkown version type");
	}
}

BundleVersion::Type BundleVersion::typefromString(const std::string& str) const
{
	if(str == "alpha") return Type::Alpha;
	else if(str == "beta") return Type::Beta;
	else if(str == "release") return Type::Release;
	else throw Version::encodingError("Unknown Version number type: " + str);
}
