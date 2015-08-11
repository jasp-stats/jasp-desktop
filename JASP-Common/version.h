#ifndef VERSION_H
#define VERSION_H

#include <string>

class Version
{
public:
	Version(unsigned char _major, unsigned char _minor, unsigned char _revision, unsigned short _build);
	Version(std::string version);
	Version();

	bool operator<(const Version&);
	bool operator>(const Version&);
	bool operator<=(const Version&);
	bool operator>=(const Version&);
	bool operator==(const Version&);
	bool operator!=(const Version&);

	bool isRelease() const;
	bool isAlpha() const;
	bool isBeta() const;
	std::string asString(bool includeRelease) const;
	bool isEmpty() const;

	unsigned char major;
	unsigned char minor;
	unsigned char revision;
	unsigned short build;
};


#endif // VERSION_H
