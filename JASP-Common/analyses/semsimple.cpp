
#include "semsimple.h"

#include "options/options.h"
#include "options/optionstring.h"
#include "options/optionboolean.h"
#include "options/optionlist.h"
#include "options/optioninteger.h"
#include "options/optionfield.h"

SEMSimple::SEMSimple(int id)
	: Analysis(id, "SEMSimple")
{
}

Options *SEMSimple::createDefaultOptions()
{
	Options *options = new Options();

	options->add("model", new OptionString());

	options->add("errorCalculation", new OptionList(list("standard", "robust", "bootstrap")));
	options->add("errorCalculationBootstrapSamples", new OptionInteger(10000));

	options->add("output/marthesCoefficient", new OptionBoolean());
	options->add("output/additionalFitMeasures", new OptionBoolean());
	options->add("output/impliedCovarianceMatrix", new OptionBoolean());
	options->add("output/observedCovarianceMatrix", new OptionBoolean());
	options->add("output/modificationIndices", new OptionBoolean());
	options->add("output/modificationIndicesHideLowIndices", new OptionBoolean());
	options->add("output/modificationIndicesHideLowIndicesThreshold", new OptionInteger());

	options->add("groupingVariable", new OptionField());
	options->add("estimator", new OptionList(list("ML", "GLS", "WLS", "ULS", "DWLS")));

	options->add("includeMeanStructure", new OptionBoolean());
	options->add("assumeFactorsUncorrelated", new OptionBoolean());
	options->add("fixExogenousCovariates", new OptionBoolean());
	options->add("factorStandardisation", new OptionList(list("none", "residualVariance", "factorLoadings")));

	options->add("fixManifestInterceptsToZero", new OptionBoolean());
	options->add("fixLatentInterceptsToZero", new OptionBoolean());
	options->add("omitResidualSingleIndicator", new OptionBoolean());
	options->add("residualVariances", new OptionBoolean());
	options->add("correlateExogenousLatents", new OptionBoolean());
	options->add("addThresholds", new OptionBoolean());
	options->add("addScalingParameters", new OptionBoolean());
	options->add("correlateDependentVariables", new OptionBoolean());

	options->add("emulation", new OptionList(list("none", "Mplus", "EQS")));

	return options;
}
