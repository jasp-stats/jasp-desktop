//
// Copyright (C) 2013-2017 University of Amsterdam
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

using namespace std;
using namespace boost;

Analysis *AnalysisLoader::load(int id, string analysisName, Json::Value *data)
{
	Options *options = new Options();

	string path = Dirs::libraryDir() + "/" + analysisName + ".json";

	nowide::ifstream file(path.c_str(), fstream::in);
	if (file.is_open())
	{
		Json::Value analysisDesc;
		Json::Reader parser;
		parser.parse(file, analysisDesc);

		Json::Value optionsJson = analysisDesc.get("options", Json::nullValue);
		if (optionsJson != Json::nullValue)
			options->init(optionsJson);
		else
			perror("malformed analysis definition");

		bool autorun = analysisDesc.get("autorun", false).asBool();
		bool usedata = analysisDesc.get("usedata", true).asBool();
		Version version = Version(analysisDesc.get("version", "0.00").asString());

		if (data != NULL)
			options->set(*data);

		file.close();

		return new Analysis(id, analysisName, options, version, autorun, usedata);
	}

	throw runtime_error("Could not access analysis definition.");
}
