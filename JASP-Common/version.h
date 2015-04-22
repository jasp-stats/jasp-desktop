#ifndef VERSION_H
#define VERSION_H

#include <string>

class Version
{
public:
	Version(unsigned char _massive, unsigned char _major, unsigned char _minor, unsigned char _revision, unsigned char _build);
	Version();
	Version(std::string version);

	static Version fromString(std::string versionString);

	bool operator<(const Version&);
	bool operator>(const Version&);
	bool operator<=(const Version&);
	bool operator>=(const Version&);
	bool operator==(const Version&);
	bool operator!=(const Version&);

	bool isRelease() const;
	std::string asString(bool includeMassive) const;
	bool isEmpty() const;

	unsigned char massive;
	unsigned char major;
	unsigned char minor;
	unsigned char revision;
	unsigned char build;
};


#endif // VERSION_H
