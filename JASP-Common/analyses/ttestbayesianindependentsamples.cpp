#include "ttestbayesianindependentsamples.h"

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

TTestBayesianIndependentSamples::TTestBayesianIndependentSamples(int id)
	: Analysis(id, "TTestBayesianIndependentSamples", createOptions())
{
}

Options *TTestBayesianIndependentSamples::createOptions() const
{
	Options *options = new Options();

	options->add("variables", new OptionVariables());
	options->add("groupingVariable", new OptionVariable());

	options->add("descriptives", new OptionBoolean());

	options->add("tails", new OptionList(list("twoTailed", "oneTailedGreaterThan", "oneTailedLessThan")));
	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));

	return options;
}

