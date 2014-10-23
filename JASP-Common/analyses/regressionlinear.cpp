
#include "regressionlinear.h"

#include "options/optionvariables.h"
#include "options/optionvariable.h"
#include "options/optionstring.h"
#include "options/optionlist.h"
#include "options/optionstable.h"
#include "options/optionboolean.h"
#include "options/optionnumber.h"
#include "options/optioninteger.h"

RegressionLinear::RegressionLinear(int id)
	: Analysis(id, "RegressionLinear", createOptions())
{
}

Options *RegressionLinear::createOptions() const
{
	Options *options = new Options();

	options->add("dependent", new OptionVariable());

	Options *layerOptionsTemplate = new Options();
	layerOptionsTemplate->add("name", new OptionString("Block %1"));
	layerOptionsTemplate->add("variables", new OptionVariables());
	layerOptionsTemplate->add("method", new OptionList(list("Enter")));//, "Stepwise", "Remove", "Backward", "Forward")));

	options->add("blocks", new OptionsTable(layerOptionsTemplate));

    options->add("wlsWeights", new OptionVariable());


    options->add("regressionCoefficients/estimates", new OptionBoolean(true));
	options->add("regressionCoefficients/confidenceIntervals", new OptionBoolean());
	options->add("regressionCoefficients/confidenceIntervalsInterval", new OptionInteger(95));
	options->add("regressionCoefficients/covarianceMatrix", new OptionBoolean());

	options->add("modelFit", new OptionBoolean(true));
	options->add("rSquaredChange", new OptionBoolean());
	options->add("descriptives", new OptionBoolean());
	options->add("partAndPartialCorrelations", new OptionBoolean());
	options->add("collinearityDiagnostics", new OptionBoolean());

	options->add("residuals/durbinWatson", new OptionBoolean());
	options->add("residuals/casewiseDiagnostics", new OptionBoolean());
	options->add("residuals/casewiseDiagnosticsType", new OptionList(list("outliersOutside", "allCases")));
	options->add("residuals/casewiseDiagnosticsOutliersOutside", new OptionInteger(3));

	options->add("steppingMethodCriteria/type", new OptionList(list("usePValue", "useFValue")));
	options->add("steppingMethodCriteria/pEntry", new OptionNumber(0.05));
	options->add("steppingMethodCriteria/pRemoval", new OptionNumber(0.10));
	options->add("steppingMethodCriteria/fEntry", new OptionNumber(3.84));
	options->add("steppingMethodCriteria/fRemoval", new OptionNumber(2.71));

	options->add("includeConstant", new OptionBoolean(true));

	options->add("missingValues", new OptionList(list("excludeCasesListwise", "excludeCasesPairwise", "replaceWithMeans")));

	return options;
}
