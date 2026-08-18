//
// Copyright (C) 2018 University of Amsterdam
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
//	NOTICE:
//		`appinfo.cpp` is generated from `appinfo.cpp.in` and you should edit
//		that file instead if you want your changes to reflect in the app
//

#ifndef INSTALLEDMODULES_H
#define INSTALLEDMODULES_H

#include <vector>
#include <string>
#include <map>
#include "version.h"

/**
 * @brief 		A minimal class for reporting the list active modules, to be used by `loadModules`.
  *
  * @details 	Reads all available shipped and installed modules and divides them into two groups common (on ribbon) and extra (selectable)
  *				The order equals the order in the modules-settings.json which specifies these groups
  *				If OverrideCommon is specified in config.toml, it will override the common modules list
  */
class InstalledModules {
public:

	struct ModuleInfo {
		std::string 	name	= "",
						libpath = "";
		bool			common	= false,
						bundled	= false;
		BundleVersion	version;

	};

	static std::vector<ModuleInfo> getAllAvailableModules();

	static std::vector<ModuleInfo> getModules();

	static std::map<std::string, std::string> getInstalledModuleVersions();

private:

	static void parseModuleInfo(const std::string& path, ModuleInfo& info);

	static const std::string settingsPath;

};


inline bool operator<(const InstalledModules::ModuleInfo & lhs, const InstalledModules::ModuleInfo & rhs)
{
	if(lhs.name != rhs.name)
		return lhs.name < rhs.name;
	
	if(lhs.version != rhs.version)
		return lhs.version < rhs.version;
	
	//If it is the same version we should prioritize the one the user installed, so this one is smaller than if it isnt bundled while the other is.
	return !lhs.bundled && rhs.bundled;
};

inline bool operator> (const InstalledModules::ModuleInfo & lhs, const InstalledModules::ModuleInfo & rhs) { return rhs < lhs; }
inline bool operator<=(const InstalledModules::ModuleInfo & lhs, const InstalledModules::ModuleInfo & rhs) { return !(lhs > rhs); }
inline bool operator>=(const InstalledModules::ModuleInfo & lhs, const InstalledModules::ModuleInfo & rhs) { return !(lhs < rhs); }

#endif // INSTALLEDMODULES_H
