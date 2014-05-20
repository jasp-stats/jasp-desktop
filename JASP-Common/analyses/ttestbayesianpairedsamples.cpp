#include "ttestbayesianpairedsamples.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionterms.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionvariablesgroups.h"

using namespace std;

TTestBayesianPairedSamples::TTestBayesianPairedSamples(int id)
	: Analysis(id, "TTestBayesianPairedSamples")
{
}

Options *TTestBayesianPairedSamples::createDefaultOptions()
{
	Options *options = new Options();

	options->add("pairs", new OptionVariablesGroups());

	options->add("descriptives", new OptionBoolean());

	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));

	return options;
}

string TTestBayesianPairedSamples::order()
{
	return "ttest,descriptives";
}
