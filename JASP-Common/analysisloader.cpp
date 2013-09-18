#include "analysisloader.h"

#include "analyses/descriptives.h"
#include "analyses/ttestonesample.h"
#include "analyses/ttestindependentsamples.h"
#include "analyses/ttestbayesianonesample.h"
#include "analyses/ttestpairedsamples.h"
#include "analyses/anovaoneway.h"
#include "analyses/anovamultivariate.h"
#include "analyses/anova.h"

#include "analysispart.h"

Analysis *AnalysisLoader::load(int id, string analysisName)
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
	else if (analysisName == "TTestPairedSamples")
	{
		return new analyses::TTestPairedSamples(id);
	}
	else if (analysisName == "TTestBayesianOneSample")
	{
		return new analyses::TTestBayesianOneSample(id);
	}
	else if (analysisName == "AnovaOneWay")
	{
		return new analyses::AnovaOneWay(id);
	}
	else if (analysisName == "AnovaMultivariate")
	{
		return new analyses::AnovaMultivariate(id);
	}
	else if (analysisName == "Anova")
	{
		return new analyses::Anova(id);
	}

    return NULL;
}
