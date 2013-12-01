
#include "regressionlinear.h"

#include "options/optionfield.h"

RegressionLinear::RegressionLinear(int id)
	: Analysis(id, "RegressionLinear")
{
}

Options *RegressionLinear::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionField("dependent"));
	options->add(new OptionFields("variables"));

	return options;
}
