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

#ifndef JASPIMPORTER_H
#define JASPIMPORTER_H


#include "../datasetpackage.h"

#include <boost/function.hpp>

#include <string>
#include <vector>
#include <QCoreApplication>
#include "utilities/qutils.h"

///
/// Loads a jasp file
/// We generally try to make sure to always stay backwards compatible as far as we can.
/// Which as of 0.15 is still all the way back to 0.8.? something at least
class JASPImporter
{
	Q_DECLARE_TR_FUNCTIONS(JASPImporter)
public:
	enum Compatibility { Compatible, Limited, NotCompatible };

	static void loadDataSet(const std::string &path, boost::function<void(int)> progressCallback);

private:
	static void loadDataArchive(		const std::string &path, boost::function<void(int)> progressCallback);
	static void loadJASPArchive(		const std::string &path, boost::function<void(int)> progressCallback);
	static void loadDataArchive_1_00(	const std::string &path, boost::function<void(int)> progressCallback);
	static void loadJASPArchive_1_00(	const std::string &path, boost::function<void(int)> progressCallback);

	static bool parseJsonEntry(Json::Value &root, const std::string &path, const std::string &entry, bool required);
	static void readManifest(const std::string &path);
	static Compatibility isCompatible();
};

#endif // JASPIMPORTER_H
