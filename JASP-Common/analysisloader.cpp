#include "analysisloader.h"

#include "dirs.h"

#include <fstream>
#include <string>

using namespace std;

Analysis *AnalysisLoader::load(int id, string analysisName)
{
	Options *options = new Options();

	string path = Dirs::libraryDir() + "/" + analysisName + ".json";

	ifstream myfile(path.c_str(), fstream::in);
	if (myfile.is_open())
	{
		Json::Value descriptiveJson;
		Json::Reader parser;
		parser.parse(myfile, descriptiveJson);

		options->loadData(descriptiveJson);

		myfile.close();

		cout << "Analysis loaded";
		cout.flush();
		return new Analysis(id, analysisName, options);
	}

	throw runtime_error("Could not access analysis definition.");
}
