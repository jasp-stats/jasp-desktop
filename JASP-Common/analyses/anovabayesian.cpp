#include "anovabayesian.h"

#include "options.h"
#include "option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionstring.h"
#include "options/optionfield.h"

using namespace analyses;

AnovaBayesian::AnovaBayesian(int id)
	: Analysis(id, "AnovaBayesian")
{
}

Options *AnovaBayesian::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionField("dependent"));
	options->add(new OptionFields("fixedFactors"));
	options->add(new OptionFields("randomFactors"));

	options->add(new OptionString("model"));

	options->add(new OptionFields("nuisanceTerms"));

	return options;
}
