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
		Version version = Version(analysisDesc.get("version", "0.00").asString());

		if (data != NULL)
			options->set(*data);

		file.close();

		return new Analysis(id, analysisName, options, version, autorun);
	}

	throw runtime_error("Could not access analysis definition.");
}
