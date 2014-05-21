
#include "anovaoneway.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionvariable.h"
#include "options/optionstable.h"

using namespace std;

AnovaOneWay::AnovaOneWay(int id)
	: Analysis(id, "AnovaOneWay")
{
}

Options *AnovaOneWay::createDefaultOptions()
{
	Options *options = new Options();

	options->add("variables", new OptionVariables());
	options->add("groupingVariable", new OptionVariable());

	options->add("postHocTestsFDR", new OptionBoolean());
	options->add("postHocTestsBenjamini", new OptionBoolean());
	options->add("postHocTestsBonferroni", new OptionBoolean());
	options->add("postHocTestsHochberg", new OptionBoolean());
	options->add("postHocTestsHolm", new OptionBoolean());
	options->add("postHocTestsHommel", new OptionBoolean());

	options->add("equalityOfVariances", new OptionList(list("assumeEqual", "assumeUnequal", "both")));

	options->add("testUnequalVariances", new OptionBoolean());

	options->add("meanDifference", new OptionBoolean());
	options->add("confidenceInterval", new OptionBoolean());
	options->add("confidenceIntervalInterval", new OptionNumber(.95, 0, 1, "%"));
	options->add("descriptives", new OptionBoolean());

	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));

	Options *contrastTemplate = new Options();
	contrastTemplate->add("A", new OptionVariables());
	contrastTemplate->add("B", new OptionVariables());

	options->add("contrasts", new OptionsTable(contrastTemplate));

	return options;
}
