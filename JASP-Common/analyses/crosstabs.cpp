#include "crosstabs.h"

#include "options/options.h"
#include "options/optionstable.h"

Crosstabs::Crosstabs(int id)
	: Analysis(id, "Crosstabs")
{
}

Options *Crosstabs::createDefaultOptions()
{
	Options *options = new Options();

	options->add("rows", new OptionFields());
	options->add("columns", new OptionFields());

	Options *layerOptionsTemplate = new Options();

	options->add("layers", new OptionsTable(layerOptionsTemplate));

	return options;
}
