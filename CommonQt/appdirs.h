//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.


//
//	NOTICE:
//		`appdirs.h` is generated from `appdirs.h.in` and you should edit 
//		that file instead if you want your changes to reflect in the app
//

#ifndef APPDIRS_H
#define APPDIRS_H

#include <QString>
#include <QDir>

/// collector class with functions that make the locations of relevannt folders available
/// Uses Qt so can't be used in R-Interface.
class AppDirs
{
public:

	static QString examples();
	static QString help();
	static QString analysisDefaultsDir();
	static QString userRLibrary();
	static QString userModulesDir();
	static QString bundledModulesDir();
	static QString documents();
	static QString logDir();
	static QString appData();
	static QString rHome();
	static QDir    programDir();
	static QString renvRootLocation();
	static QString renvCacheLocations();
	
private:
	static QString processPath(const QString & path);

#ifdef linux
	static QString rHomeDir() {
		return QString("/Users/brunoboutin/JASP/source/build-jasp-desktop-Qt_6_2_4_for_macOS-Debug/Frameworks/R.framework/Versions/4.1/Resources");
	} 
#endif
};

#endif // APPDIRS_H
