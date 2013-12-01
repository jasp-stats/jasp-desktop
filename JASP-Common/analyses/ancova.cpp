#include "ancova.h"

#include "options.h"
#include "option.h"
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

	options->add(new OptionField("dependent"));
	options->add(new OptionFields("fixedFactors"));
	options->add(new OptionFields("randomFactors"));
	options->add(new OptionFields("covariates"));
	options->add(new OptionField("wlsWeights"));

	options->add(new OptionFields("modelTerms"));

	return options;
}

