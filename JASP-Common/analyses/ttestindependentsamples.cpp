#include "ttestindependentsamples.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariable.h"
#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"

using namespace std;

TTestIndependentSamples::TTestIndependentSamples(int id)
	: Analysis(id, "TTestIndependentSamples", createOptions())
{
}

Options *TTestIndependentSamples::createOptions() const
{
	Options *options = new Options();

	options->add("variables", new OptionVariables());
	options->add("groupingVariable", new OptionVariable());

	options->add("equalityOfVariances", new OptionList(list("assumeEqual", "noAssumption", "reportBoth")));

	options->add("testUnequalVariances", new OptionBoolean());

	options->add("meanDifference", new OptionBoolean());
	options->add("confidenceInterval", new OptionBoolean());
	options->add("confidenceIntervalInterval", new OptionNumber(.95, 0, 1, "%"));
	options->add("descriptives", new OptionBoolean());

	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));
	options->add("tails", new OptionList(list("twoTailed", "oneailedGreaterThan", "oneTailedLessThan")));

	return options;
}

