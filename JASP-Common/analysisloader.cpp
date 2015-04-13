#include "analysisloader.h"

#include <string>
#include <boost/nowide/fstream.hpp>

#include "dirs.h"

using namespace std;
using namespace boost;

Analysis *AnalysisLoader::load(int id, string analysisName, Json::Value *data, Analysis::Status status)
{
	Options *options = new Options();

	string path = Dirs::libraryDir() + "/" + analysisName + ".json";

	nowide::ifstream file(path.c_str(), fstream::in);
	if (file.is_open())
	{
		Json::Value analysisDesc;
		Json::Reader parser;
		parser.parse(file, analysisDesc);

		bool autorun = true;

		if (analysisDesc.isArray())
		{
			options->init(analysisDesc);
		}
		else
		{
			Json::Value optionsJson = analysisDesc.get("options", Json::nullValue);
			if (optionsJson != Json::nullValue)
				options->init(optionsJson);
			else
				perror("malformed analysis definition");

			autorun = analysisDesc.get("autorun", false).asBool();
		}

		if (data != NULL)
			options->set(*data);

		file.close();

		return new Analysis(id, analysisName, options, autorun, status);
	}

	throw runtime_error("Could not access analysis definition.");
}
