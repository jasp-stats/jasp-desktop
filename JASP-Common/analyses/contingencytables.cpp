
#include "contingencytables.h"

#include "options/optionfields.h"

ContingencyTables::ContingencyTables(int id)
	: Analysis(id, "ContingencyTables")
{
}

Options *ContingencyTables::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("rows"));
	options->add(new OptionFields("columns"));

	return options;
}
