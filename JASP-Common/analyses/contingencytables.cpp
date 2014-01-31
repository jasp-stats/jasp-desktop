
#include "contingencytables.h"

#include "options/optionfields.h"
#include "options/optionstable.h"

ContingencyTables::ContingencyTables(int id)
	: Analysis(id, "ContingencyTables")
{
}

Options *ContingencyTables::createDefaultOptions()
{
	Options *options = new Options();

	options->add("rows", new OptionFields());
	options->add("columns", new OptionFields());

	OptionsRow *layerOptionsTemplate = new OptionsRow();

	options->add("layers", new OptionsTable(layerOptionsTemplate));

	return options;
}
