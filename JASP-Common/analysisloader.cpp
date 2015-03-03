#include "analysisloader.h"

#include "dirs.h"

Analysis *AnalysisLoader::load(int id, string analysisName)
{
	Options *options = new Options();

	std::ifstream myfile(Dirs::libraryDir() + "/" + analysisName + ".json");
	if (myfile.is_open())
	{
		Json::Value descriptiveJson;
		Json::Reader parser;
		parser.parse(myfile, descriptiveJson);

		options->loadData(descriptiveJson);

		myfile.close();

		cout << "Analysis loaded";
		std::cout.flush();
		return new Analysis(id, analysisName, options);
	}

	throw std::runtime_error("Could not access analysis definition.");
}
