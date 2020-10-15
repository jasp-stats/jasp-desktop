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

#include "../Common/version.h"
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
		build = 255 + buildIndex;
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

			if (hasBuild)
			{
				if (hasBuildIndex == true && strcmp(buildString, "Alpha") == 0)
					build = buildIndex;
				else if(hasBuildIndex == true && strcmp(buildString, "Beta") == 0)
					build = 100 + buildIndex;
				else if (hasBuildIndex == true && strcmp(buildString, "Release") == 0)
					build = 255 + buildIndex;
				else
					error = true;
			}
			else
				build = 255;
		}
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

bool Version::isRelease() const
{
	return build >= 255;
}

bool Version::isAlpha() const
{
	return build >= 1 && build <= 100;
}

bool Version::isBeta() const
{
	return build >= 101 && build <= 254;
}

std::string Version::asString(bool addDebugFlag) const
{
	std::stringstream stream;

	stream << (int)major << "." << (int)minor;

	if (revision != 0 || build > 255 )
		stream << "." << (int)revision;

	/*if (isRelease()) {
		if (build > 255)
			stream << "." << (int)(build - 255);
	}
	else if (isAlpha())
		stream << " Alpha " << (int)build;
	else if (isBeta())
		stream << " Beta " << (int)(build - 100);*/

	if(addDebugFlag) //Moved from aboutmodel.cpp
	{
#ifdef JASP_DEBUG
		stream << "-Debug";
#endif

	}

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
