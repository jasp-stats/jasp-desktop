#include "ttestpairedsamples.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionterms.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionvariables.h"
#include "options/optionvariablesgroups.h"

using namespace std;

TTestPairedSamples::TTestPairedSamples(int id)
	: Analysis(id, "TTestPairedSamples", createOptions())
{
}

Options *TTestPairedSamples::createOptions() const
{
	Options *options = new Options();

	options->add("pairs", new OptionVariablesGroups());

	options->add("meanDifference", new OptionBoolean());
	options->add("confidenceInterval", new OptionBoolean());
	options->add("confidenceIntervalInterval", new OptionNumber(.95, 0, 1, "%"));
	options->add("descriptives", new OptionBoolean());
	options->add("effectSize", new OptionBoolean());

	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));
	options->add("tails", new OptionList(list("twoTailed", "oneTailedGreaterThan", "oneTailedLessThan")));

	return options;
}
