#ifndef APPINFO_H
#define APPINFO_H

#include "version.h"

class AppInfo
{
public:
	static const Version version;
	static const std::string name;

	static std::string getShortDesc();
};

#endif // APPINFO_H

