#include "ancova.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionfield.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"

Ancova::Ancova(int id)
	: Analysis(id, "Ancova")
{
}

Options *Ancova::createDefaultOptions()
{
	Options *options = new Options();

	options->add("dependent", new OptionField());
	options->add("fixedFactors", new OptionFields());
	options->add("randomFactors", new OptionFields());
	options->add("covariates", new OptionFields());
	options->add("wlsWeights", new OptionField());

	options->add("modelTerms", new OptionFields());

	return options;
}

