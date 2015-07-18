#include "appinfo.h"

const Version AppInfo::version = Version(0, 0, 7, 1, 103);
const std::string AppInfo::name = "JASP";

std::string AppInfo::getShortDesc(bool includeMassive)
{
	return AppInfo::name + " " + AppInfo::version.asString(includeMassive, true);
}
