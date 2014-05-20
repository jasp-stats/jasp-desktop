#include "anovamultivariate.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariables.h"
#include "options/optionvariable.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"

AnovaMultivariate::AnovaMultivariate(int id)
	: Analysis(id, "AnovaMultivariate")
{
}

Options *AnovaMultivariate::createDefaultOptions()
{
	Options *options = new Options();

	options->add("dependents", new OptionVariables());
	options->add("fixedFactors", new OptionVariables());
	options->add("wlsWeights", new OptionVariable());

	options->add("model", new OptionString());

	return options;
}
