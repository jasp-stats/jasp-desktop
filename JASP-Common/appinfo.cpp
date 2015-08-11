#include "appinfo.h"


const Version AppInfo::version = Version(0, 7, 1, 256);
const std::string AppInfo::name = "JASP";

std::string AppInfo::getShortDesc()
{
	return AppInfo::name + " " + AppInfo::version.asString(false);
}
