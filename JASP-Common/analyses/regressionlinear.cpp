
#include "regressionlinear.h"

#include "options/optionfield.h"

RegressionLinear::RegressionLinear(int id)
	: Analysis(id, "RegressionLinear")
{
}

Options *RegressionLinear::createDefaultOptions()
{
	Options *options = new Options();

	options->add("dependent", new OptionField());
	options->add("variables", new OptionFields());

	return options;
}
