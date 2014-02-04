
#include "semsimple.h"

#include "options/options.h"
#include "options/optionstring.h"

SEMSimple::SEMSimple(int id)
	: Analysis(id, "SEMSimple")
{
}

Options *SEMSimple::createDefaultOptions()
{
	Options *options = new Options();

	options->add("model", new OptionString());

	return options;
}
