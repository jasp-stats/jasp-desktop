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

#include "analysisloader.h"

#include <string>
#include <boost/nowide/fstream.hpp>

#include "dirs.h"
#include "version.h"
#include "appinfo.h"

using namespace std;
using namespace boost;

Analysis *AnalysisLoader::load(int id, string moduleName, string analysisName, const Version &version, Json::Value *data)
{
	bool autorun = true;
	bool usedata = true;
	bool fromQML = true;
	bool usesJaspResults = false;

	string analysisTitle = analysisName;
	Json::Value requiresInit = Json::nullValue;
	Json::Value dataKey = Json::nullValue;
	Json::Value stateKey = Json::nullValue;
	Json::Value resultsMeta = Json::nullValue;
	Json::Value optionsJson = Json::nullValue;
	
	string path = Dirs::libraryDir() + "/" + analysisName + ".json";
	nowide::ifstream file(path.c_str(), fstream::in);
	if (file.is_open())
	{
		fromQML = false;
		Json::Value analysisDesc;
		Json::Reader parser;
		parser.parse(file, analysisDesc);
		
		analysisTitle		= analysisDesc.get("title",			Json::nullValue).asString();
		requiresInit		= analysisDesc.get("init",			Json::nullValue);
		dataKey				= analysisDesc.get("dataset",		Json::nullValue);
		stateKey			= analysisDesc.get("state",			Json::nullValue);
		resultsMeta			= analysisDesc.get("results",		Json::nullValue);
		optionsJson			= analysisDesc.get("options",		Json::nullValue);
		usesJaspResults		= analysisDesc.get("jaspResults",	false).asBool();
		autorun				= analysisDesc.get("autorun",		false).asBool();
		usedata				= analysisDesc.get("usedata",		true).asBool();

		file.close();
	}
	
	if (fromQML)
	{
		// Temporary solution!!
		if (analysisName.substr(0,3) == "QML")
			analysisName = analysisName.substr(3);
	}
	

	return new Analysis(id, moduleName, analysisName, analysisTitle, requiresInit, dataKey, stateKey, resultsMeta, optionsJson, version, data, autorun, usedata, fromQML, usesJaspResults);
}
