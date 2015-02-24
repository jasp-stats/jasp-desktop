#include "crosstabs.h"

#include "options/options.h"
#include "options/optionstable.h"
#include "options/optionstring.h"
#include "options/optionboolean.h"
#include "options/optionnumber.h"
#include "options/optioninteger.h"
#include "options/optionlist.h"
#include "options/optionvariables.h"
#include "options/optionvariable.h"

Crosstabs::Crosstabs(int id)
	: Analysis(id, "Crosstabs", createOptions())
{
}

Options *Crosstabs::createOptions() const
{
	Options *options = new Options();

	options->add("rows", new OptionVariables());
	options->add("columns", new OptionVariables());
	options->add("counts", new OptionVariable());

	Options *layerOptionsTemplate = new Options();
	layerOptionsTemplate->add("name", new OptionString("Layer %1"));
	layerOptionsTemplate->add("variables", new OptionVariables());

	options->add("layers", new OptionsTable(layerOptionsTemplate));

	options->add("chiSquared", new OptionBoolean(true));
	options->add("chiSquaredContinuityCorrection", new OptionBoolean(false));
	options->add("likelihoodRatio", new OptionBoolean(false));
	options->add("oddsRatio", new OptionBoolean(false));
	options->add("oddsRatioConfidenceIntervalInterval", new OptionNumber(.95, 0, 1, "%"));

	options->add("nominal/contingencyCoefficient", new OptionBoolean());
	options->add("nominal/phiAndCramersV", new OptionBoolean());
	options->add("nominal/lambda", new OptionBoolean());
	options->add("nominal/uncertaintyCoefficient", new OptionBoolean());

	options->add("ordinal/gamma", new OptionBoolean());
	options->add("ordinal/somersD", new OptionBoolean());
	options->add("ordinal/kendallsTauB", new OptionBoolean());
	options->add("ordinal/kendallsTauC", new OptionBoolean());

	options->add("nominalByInterval/Eta", new OptionBoolean());

	options->add("cochransAndMantel", new OptionBoolean());
	options->add("testOddsRatioEquals", new OptionNumber(1));

	options->add("countsObserved", new OptionBoolean(true));
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
