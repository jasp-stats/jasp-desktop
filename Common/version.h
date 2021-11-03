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

#ifndef VERSION_H
#define VERSION_H

#include <string>
#include <stdexcept>

///
/// Stores versions and has flexible parsing from string to instantiate.
/// The fourth value wat used as a sort of alpha/beta flagg at some point in the past but it added a lot of crap and we didn't use it so I removed it.
/// To be able to read those values it is still there, vestigially.
class Version
{
public:
	struct encodingError  : public std::runtime_error
	{
		encodingError(std::string versionStr) : std::runtime_error("Module version not understood: " + versionStr) {}
		const char* what() const noexcept override;
	};

	Version(std::string version);
	Version(unsigned int major = 0, unsigned int minor = 0, unsigned int release = 0, unsigned int fourth = 0) : _major(major), _minor(minor), _release(release), _fourth(fourth) {}

	///By default this tries to minimize the string, so all trailing zeroes + dots are removed. Unless versionNumbersToInclude is set to something >1. If 2 then major and minor are always shown, etc.
	std::string asString(size_t versionNumbersToInclude = 0) const;


	unsigned int major()	const { return _major; }
	unsigned int minor()	const { return _minor; }
	unsigned int release()	const { return _release; }
	unsigned int fourth()	const { return _fourth; }

	bool		isEmpty() const;

	void		swap(Version &other );

	bool		operator <	(const Version & other) const;
	bool		operator <=	(const Version & other) const;
	bool		operator >=	(const Version & other) const;
	bool		operator >	(const Version & other) const;
	bool		operator ==	(const Version & other) const;
	bool		operator !=	(const Version & other) const;


private:
	unsigned int	_major		= 0,
					_minor		= 0,
					_release	= 0,
					_fourth		= 0; //This is just here to be able to parse older files...
};

#endif // VERSION_H
