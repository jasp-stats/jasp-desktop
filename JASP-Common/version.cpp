#include "version.h"
#include <string.h>
#include <iomanip>
#include <sstream>
#include <stdio.h>

using namespace std;

Version::Version()
{

}

Version::Version(unsigned char _massive, unsigned char _major, unsigned char _minor, unsigned char _revision, unsigned char _build)
{
	massive = _massive;
	major = _major;
	minor = _minor;
	revision = _revision;
	build = _build;
}

Version::Version(std::string versionString)
{
	char buildString[8];
	unsigned char buildIndex = 0;

	int v1 = sscanf(versionString.c_str(), "%hhu.%hhu.%hhu.%hhu %7s %hhu", &massive, &major, &minor, &revision, buildString, &buildIndex);
	bool hasMassive = v1 >= 4;
	bool hasRevision = true;

	if ( ! hasMassive)
	{
		massive = 0;
		v1 = sscanf(versionString.c_str(), "%hhu.%hhu.%hhu %7s %hhu", &major, &minor, &revision, buildString, &buildIndex);
		hasRevision = v1 >= 3;
		if ( ! hasRevision)
		{
			revision = 0;
			v1 = sscanf(versionString.c_str(), "%hhu.%hhu %7s %hhu", &major, &minor, buildString, &buildIndex);
		}
	}




	bool error = v1 <= 0;
	if (! error)
	{
		bool hasBuild = hasMassive ? v1 > 4 : v1 > 3;
		bool hasBuildIndex = hasMassive ? v1 == 6 : v1 == 5;
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
			else if (hasBuildIndex == false && strcmp(buildString, "Release") == 0)
				build = 255;
			else
				error = true;
		}
		else
			build = 255;
	}

	if (error)
	{
		massive = 0;
		major = 0;
		minor = 0;
		revision = 0;
		build = 0;
	}
}

bool Version::operator>(const Version& version)
{
	return ((this->massive > version.massive) ||
		(this->massive == version.massive && this->major > version.major) ||
		(this->massive == version.massive && this->major == version.major && this->minor > version.minor) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision > version.revision) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build > version.build));
}

bool Version::operator<(const Version& version)
{
	return ((this->massive < version.massive) ||
		(this->massive == version.massive && this->major < version.major) ||
		(this->massive == version.massive && this->major == version.major && this->minor < version.minor) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision < version.revision) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build < version.build));
}

bool Version::operator>=(const Version& version)
{
	return ((this->massive > version.massive) ||
		(this->massive == version.massive && this->major > version.major) ||
		(this->massive == version.massive && this->major == version.major && this->minor > version.minor) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision > version.revision) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build >= version.build));
}

bool Version::operator<=(const Version& version)
{
	return ((this->massive < version.massive) ||
		(this->massive == version.massive && this->major < version.major) ||
		(this->massive == version.massive && this->major == version.major && this->minor < version.minor) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision < version.revision) ||
		(this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build <= version.build));
}

bool Version::operator==(const Version& version)
{
	return this->massive == version.massive && this->major == version.major && this->minor == version.minor && this->revision == version.revision && this->build == version.build;
}

bool Version::operator!=(const Version& version)
{
	return this->massive != version.massive || this->major != version.major || this->minor != version.minor || this->revision != version.revision || this->build != version.build;
}

bool Version::isRelease() const
{
	return build == 255;
}

string Version::asString(bool includeMassive, bool includeRelease) const
{
	stringstream stream;
	if (includeMassive)
		stream << (int)massive << ".";

	stream << (int)major << ".";
	//stream << std::setfill('0') << std::setw(2) << std::right;
	stream << (int)minor;
	if (revision != 0)
		stream << "." << (int)revision;

	if (build == 255 && includeRelease)
		stream << " Release";
	else if (build >= 1 && build <= 100)
		stream << " Alpha " << (int)build;
	else if (build >= 101 && build <= 245)
		stream << " Beta " << (int)(build - 100);

	return stream.str();
}

bool Version::isEmpty() const
{
	return massive == 0 &&
		major == 0 &&
		minor == 0 &&
		revision == 0 &&
		build == 0;
}
