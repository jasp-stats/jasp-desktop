#include "correlation.h"

#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"

Correlation::Correlation(int id)
	: Analysis(id, "Correlation", createOptions())
{
}

Options *Correlation::createOptions() const
{
	Options *options = new Options();

	options->add("variables", new OptionVariables());

	options->add("pearson", new OptionBoolean(true));
	options->add("kendallsTauB", new OptionBoolean());
	options->add("spearman", new OptionBoolean());

	options->add("hypothesis", new OptionList(list("correlated", "correlatedInDirection")));

	options->add("reportSignificance", new OptionBoolean());
	options->add("flagSignificant", new OptionBoolean());

	options->add("confidenceIntervals", new OptionBoolean());
	options->add("confidenceIntervalsInterval", new OptionNumber(.95, 0, 1, "%"));

	options->add("meansAndStdDev", new OptionBoolean());
	options->add("crossProducts", new OptionBoolean());

	options->add("missingValues", new OptionList(list("excludePairwise", "excludeListwise")));

	return options;
}
