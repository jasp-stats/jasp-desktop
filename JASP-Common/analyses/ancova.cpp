#include "ancova.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariable.h"
#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"

Ancova::Ancova(int id)
	: Analysis(id, "Ancova", createOptions())
{
}

Options *Ancova::createOptions() const
{
	Options *options = new Options();

	options->add("dependent", new OptionVariable());
	options->add("fixedFactors", new OptionVariables());
	options->add("randomFactors", new OptionVariables());
	options->add("covariates", new OptionVariables());
	options->add("wlsWeights", new OptionVariable());

	options->add("modelTerms", new OptionVariables());

	return options;
}

