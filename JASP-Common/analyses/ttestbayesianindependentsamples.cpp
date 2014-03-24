#include "ttestbayesianindependentsamples.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionfield.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"

using namespace std;

TTestBayesianIndependentSamples::TTestBayesianIndependentSamples(int id)
	: Analysis(id, "TTestBayesianIndependentSamples")
{
}

Options *TTestBayesianIndependentSamples::createDefaultOptions()
{
	Options *options = new Options();

	options->add("variables", new OptionFields());
	options->add("groupingVariable", new OptionField());

	options->add("descriptives", new OptionBoolean());

	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));

	return options;
}

string TTestBayesianIndependentSamples::order()
{
	return "ttest,descriptives";
}
