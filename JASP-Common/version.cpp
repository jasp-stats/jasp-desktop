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

#include "version.h"
#include <string.h>
#include <iomanip>
#include <sstream>
#include <stdio.h>

//For https://github.com/jasp-stats/INTERNAL-jasp/issues/264 (major and minor defined by GNU C):
#ifdef major
#undef major
#endif

#ifdef minor
#undef minor
#endif

Version::Version(unsigned char _major, unsigned char _minor, unsigned char _revision, unsigned short _build)
	: major(_major), minor(_minor), revision(_revision), build(_build)
{}

Version::Version(std::string versionString)
{
	char buildString[8];
	unsigned short buildIndex = 0;

	int v0 = sscanf(versionString.c_str(), "%hhu.%hhu.%hhu.%hu", &major, &minor, &revision, &buildIndex);
	bool error = v0 <= 0;

	if (v0 == 4 && !error)
	{
		build = buildIndex;
	}
	else if (v0 < 4 && !error)
	{
		int v1 = sscanf(versionString.c_str(), "%hhu.%hhu.%hhu %7s %hu", &major, &minor, &revision, buildString, &buildIndex);
		bool hasRevision = v1 >= 3;
		if ( ! hasRevision)
		{
			revision = 0;
			v1 = sscanf(versionString.c_str(), "%hhu.%hhu %7s %hu", &major, &minor, buildString, &buildIndex);
		}

		error = v1 <= 0;
		if (! error)
		{
			bool hasBuild = v1 > 3;
			bool hasBuildIndex = v1 == 5;
			if ( ! hasRevision)
			{
				hasBuild = v1 > 2;
				hasBuildIndex = v1 == 4;
			}

			if (hasBuildIndex)	build = buildIndex;
			else				error = true;
		}
		else
			build = 0;
	}

	if (error)
	{
		major		= 0;
		minor		= 0;
		revision	= 0;
		build		= 0;
	}
}

bool Version::operator>(const Version& version) const
{
	return ((this->major > version.major) ||
			(this->major == version.major && this->minor > version.minor) ||
			(this->major == version.major && this->minor == version.minor && this->revision > version.revision) ||
			(this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build > version.build));
}

bool Version::operator<(const Version& version) const
{
	return ((this->major < version.major) ||
			(this->major == version.major && this->minor < version.minor) ||
			(this->major == version.major && this->minor == version.minor && this->revision < version.revision) ||
			(this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build < version.build));
}

bool Version::operator>=(const Version& version) const
{
	return ((this->major > version.major) ||
			(this->major == version.major && this->minor > version.minor) ||
			(this->major == version.major && this->minor == version.minor && this->revision > version.revision) ||
			(this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build >= version.build));
}

bool Version::operator<=(const Version& version) const
{
	return ((this->major < version.major) ||
			(this->major == version.major && this->minor < version.minor) ||
			(this->major == version.major && this->minor == version.minor && this->revision < version.revision) ||
			(this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build <= version.build));
}

bool Version::operator==(const Version& version) const
{
	return this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build == version.build;
}

bool Version::operator!=(const Version& version) const
{
	return this->major != version.major || this->minor != version.minor || this->revision != version.revision || this->build != version.build;
}

std::string Version::asString(bool addDebugFlag) const
{
	std::stringstream stream;

	stream << (int)major << "." << (int)minor;

	if (revision != 0)
		stream << "." << (int)revision;

	if (build > 0)
			stream << "." << (int)build;

#ifdef JASP_DEBUG
	if(addDebugFlag)
		stream << "-Debug";
#endif

	return stream.str();
}

bool Version::isEmpty() const
{
	return
		major		== 0 &&
		minor		== 0 &&
		revision	== 0 &&
		build		== 0;
}
