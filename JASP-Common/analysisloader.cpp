#include "analysisloader.h"

#include "options.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optionfield.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"

#include "analyses/descriptives.h"
#include "analyses/ttestonesample.h"
#include "analyses/ttestindependentsamples.h"
#include "analyses/ttestbayesonesample.h"
#include "analyses/anovaoneway.h"

#include "analysispart.h"

Analysis *AnalysisLoader::load(int id, string analysisName, Options *options)
{

	if (analysisName == "Descriptives")
	{
		return new analyses::Descriptives(id);
    }
	else if (analysisName == "TTestOneSample")
	{
		return new analyses::TTestOneSample(id);
	}
	else if (analysisName == "TTestIndependentSamples")
	{
		return new analyses::TTestIndependentSamples(id);
	}
	else if (analysisName == "TTestBayesOneSample")
	{
		return new analyses::TTestBayesOneSample(id);
	}
	else if (analysisName == "AnovaOneWay")
	{
		return new analyses::AnovaOneWay(id);
	}
	/*else if (analysisName == "ANOVA")
	{
		Options *options = new Options();
		options->add(new OptionFields("variables"));
		options->add(new OptionField("groupingVariable"));

		Analysis *a = new Analysis(id);

		return a;
	}*/

    return NULL;
}
