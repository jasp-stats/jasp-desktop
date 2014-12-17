#include "crosstabsbayesian.h"

#include "options/options.h"
#include "options/optionstable.h"
#include "options/optionstring.h"
#include "options/optionboolean.h"
#include "options/optionnumber.h"
#include "options/optioninteger.h"
#include "options/optionlist.h"
#include "options/optionvariables.h"
#include "options/optionvariable.h"

CrosstabsBayesian::CrosstabsBayesian(int id)
	: Analysis(id, "CrosstabsBayesian", createOptions())
{
}

Options *CrosstabsBayesian::createOptions() const
{
	Options *options = new Options();

	options->add("rows", new OptionVariables());
	options->add("columns", new OptionVariables());
	options->add("counts", new OptionVariable());

	Options *layerOptionsTemplate = new Options();
	layerOptionsTemplate->add("name", new OptionString("Layer %1"));
	layerOptionsTemplate->add("variables", new OptionVariables());

	options->add("layers", new OptionsTable(layerOptionsTemplate));

	options->add("oddsRatio", new OptionBoolean());
	options->add("oddsRatioCredibleInterval", new OptionBoolean());
	options->add("oddsRatioCredibleIntervalInterval", new OptionNumber(.95, 0, 1, "%"));

	options->add("bfIndependence", new OptionBoolean());

	options->add("samplingModel", new OptionList(list("poisson", "jointMultinomial", "independentMultinomialRowsFixed", "independentMultinomialColumnsFixed", "hypergeometric"), "jointMultinomial"));
	options->add("priorConcentration", new OptionNumber(1, 0.5, 2));

	options->add("plotPosteriorOddsRatio", new OptionBoolean());


	options->add("countsObserved", new OptionBoolean());
	options->add("countsExpected", new OptionBoolean());
	options->add("hideSmallCounts", new OptionBoolean());
	options->add("hideSmallCountsLessThan", new OptionInteger(5));

	options->add("zTest/compareColumns", new OptionBoolean());
	options->add("zTest/adjustPValues", new OptionBoolean());

	options->add("percentages/row", new OptionBoolean());
	options->add("percentages/column", new OptionBoolean());
	options->add("percentages/total", new OptionBoolean());

	options->add("residuals/unstandardized", new OptionBoolean());
	options->add("residuals/standardized", new OptionBoolean());
	options->add("residuals/adjustedStandardized", new OptionBoolean());

	options->add("rowOrder", new OptionList(list("ascending", "descending")));

	return options;
}
