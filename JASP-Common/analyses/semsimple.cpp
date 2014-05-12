
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
	options->add("errorCalculationBootstrapSamples", new OptionInteger(1000));

	options->add("output/mardiasCoefficients", new OptionBoolean());
	options->add("output/additionalFitMeasures", new OptionBoolean());
	options->add("output/fittedCovarianceCorrelations", new OptionBoolean());
	options->add("output/residualCovarianceCorrelations", new OptionBoolean());
	options->add("output/observedCovarianceCorrelations", new OptionBoolean());
	options->add("output/modificationIndices", new OptionBoolean());
	options->add("output/modificationIndicesHideLowIndices", new OptionBoolean());
	options->add("output/modificationIndicesHideLowIndicesThreshold", new OptionInteger(10));

	options->add("groupingVariable", new OptionField());
    options->add("estimator", new OptionList(list("automatic", "ML", "GLS", "WLS", "ULS", "DWLS")));

	options->add("includeMeanStructure", new OptionBoolean());
	options->add("assumeFactorsUncorrelated", new OptionBoolean());
	options->add("fixExogenousCovariates", new OptionBoolean(true));
    options->add("factorStandardisation", new OptionList(list("residualVariance", "factorLoadings", "none")));

	options->add("fixManifestInterceptsToZero", new OptionBoolean(false));
	options->add("fixLatentInterceptsToZero", new OptionBoolean(true));
	options->add("omitResidualSingleIndicator", new OptionBoolean(true));
	options->add("residualVariances", new OptionBoolean(true));
	options->add("correlateExogenousLatents", new OptionBoolean(true));
    options->add("addThresholds", new OptionBoolean(true));
	options->add("addScalingParameters", new OptionBoolean(true));
	options->add("correlateDependentVariables", new OptionBoolean(true));

    options->add("emulation", new OptionList(list("none", "Mplus", "EQS")));

    return options;
}

std::string SEMSimple::order()
{
    return "fit,parameterEstimates,fitMeasures_modelTest,fitMeasures_vsBaseline,fitMeasures_likelihoodInfo,fitMeasures_RMSEA,fitMeasures_RMR,fitMeasures_Other,covcor,modificationIndices,mardiasCoefficient";
}
