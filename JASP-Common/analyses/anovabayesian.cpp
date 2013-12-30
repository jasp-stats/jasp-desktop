#include "anovabayesian.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionfield.h"

AnovaBayesian::AnovaBayesian(int id)
	: Analysis(id, "AnovaBayesian")
{
}

Options *AnovaBayesian::createDefaultOptions()
{
	Options *options = new Options();

	options->add("dependent", new OptionField());
	options->add("fixedFactors", new OptionFields());
	options->add("randomFactors", new OptionFields());

	options->add("modelTerms", new OptionFields());

	options->add("nuisanceTerms", new OptionFields());

	return options;
}
