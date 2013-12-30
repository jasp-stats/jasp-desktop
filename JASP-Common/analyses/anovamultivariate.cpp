#include "anovamultivariate.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionfields.h"
#include "options/optionfield.h"
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

	options->add("dependents", new OptionFields());
	options->add("fixedFactors", new OptionFields());
	options->add("wlsWeights", new OptionField());

	options->add("model", new OptionString());

	return options;
}
