#include "ancovamultivariate.h"

#include "options.h"
#include "option.h"
#include "options/optionfields.h"
#include "options/optionfield.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"

AncovaMultivariate::AncovaMultivariate(int id)
	: Analysis(id, "AncovaMultivariate")
{
}

Options *AncovaMultivariate::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("dependents"));
	options->add(new OptionFields("fixedFactors"));
	options->add(new OptionFields("covariates"));
	options->add(new OptionField("wlsWeights"));

	options->add(new OptionString("model"));

	return options;
}
