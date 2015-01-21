#include "correlationbayesianpairs.h"

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

CorrelationBayesianPairs::CorrelationBayesianPairs(int id)
	: Analysis(id, "CorrelationBayesianPairs", createOptions())
{
}

Options *CorrelationBayesianPairs::createOptions() const
{
	Options *options = new Options();

	options->add("pairs", new OptionVariablesGroups());

	options->add("hypothesis", new OptionList(list("correlated", "correlatedPositively", "correlatedNegatively")));
	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));

	options->add("priorWidth", new OptionNumber(1));

	options->add("plotPriorAndPosterior", new OptionBoolean());
	options->add("plotBayesFactorRobustness", new OptionBoolean());
	options->add("plotSequentialAnalysis", new OptionBoolean());
	options->add("plotSequentialAnalysisRobustness", new OptionBoolean());

	options->add("plotWidth", new OptionInteger(320));
	options->add("plotHeight", new OptionInteger(240));

	options->add("bayesFactorType", new OptionList(list("BF10", "BF01")));

	return options;
}

