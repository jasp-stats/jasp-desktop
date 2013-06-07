#include "analysisloader.h"

#include "options.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optionfield.h"

#include "analysispart.h"

Analysis *AnalysisLoader::load(int id, string analysisName)
{

	if (analysisName == "Descriptives")
    {
		Options *options = new Options();
		options->add(new OptionFields("main/fields"));

		options->add(new OptionBoolean("statistics/centralTendency/mean"));
		options->add(new OptionBoolean("statistics/centralTendency/median"));
		options->add(new OptionBoolean("statistics/centralTendency/mode"));
		options->add(new OptionBoolean("statistics/centralTendency/sum"));

		Analysis *a = new Analysis(id, "Descriptives", options);

		return a;
    }
	else if (analysisName == "TTestOneSample")
	{
		Options *options = new Options();
		options->add(new OptionFields("main/fields"));

		Analysis *a = new Analysis(id, "TTestOneSample", options);

		return a;
	}

    return NULL;
}
