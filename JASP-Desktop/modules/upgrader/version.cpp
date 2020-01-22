#include "modules/upgrader/version.h"
#include <regex>
#include <sstream>

namespace Modules
{

const char * Version::encodingError::what() const noexcept
{
	//Just here to have an out-of-line virtual method so that clang and gcc don't complain so much
	return std::runtime_error::what();
}


Version::Version(std::string version)
{
	const static std::regex parseIt("(\\d+)(\\.(\\d+))?(\\.(\\d+))?"); //(sub)groups: 1 3 5 //0 is whole match/line

	std::smatch found;
	if(!std::regex_match(version, found, parseIt))
		throw encodingError(version);

	std::string majorStr	= found[1],
				minorStr	= found[3],
				releaseStr	= found[5];


	try
	{
		if(majorStr		!= "")	_major		= std::stoul(majorStr);
		if(minorStr		!= "")	_minor		= std::stoul(minorStr);
		if(releaseStr	!= "")	_release	= std::stoul(releaseStr);
	}
	catch(...) { throw encodingError(version); }
}

std::string Version::toString() const
{
	std::stringstream out;
										out			<< std::to_string(_major);
	if(_release > 0 || _minor > 0)		out << "."	<< std::to_string(_minor);
	if(_release > 0)					out << "."	<< std::to_string(_release);

	return out.str();
}

void Version::swap(Version &other)
{
	std::swap(_major,	other._major	);
	std::swap(_minor,	other._minor	);
	std::swap(_release, other._release	);
}

Version & Version::operator = (const Version & other)
{
	_major		= other._major;
	_minor		= other._minor;
	_release	= other._release;

	return *this;
}

bool	Version::operator ==	(const Version & other) const {	return !operator!=(other);						}
bool	Version::operator <=	(const Version & other) const {	return operator==(other) || operator<(other);	}
bool	Version::operator >=	(const Version & other) const {	return !operator<(other);						}
bool	Version::operator >		(const Version & other) const { return operator!=(other) && operator>=(other);	}

bool Version::operator !=	(const Version & other) const
{
	return _major != other._major || _minor != other._minor || _release != other._release;
}

bool Version::operator <	(const Version & other) const
{
	if(_major <	other._major)	return true;
	if(_major >	other._major)	return false;

	if(_minor < other._minor)	return true;
	if(_minor > other._minor)	return false;

	if(_release < other._release) return true;

	return false;
}

}
