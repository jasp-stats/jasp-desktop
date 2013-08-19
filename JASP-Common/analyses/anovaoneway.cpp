#include "anovaoneway.h"

#include "options.h"
#include "option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionfield.h"

#include "rinterface.h"

using namespace Json;
using namespace analyses;

AnovaOneWay::AnovaOneWay(int id)
	: Analysis(id, "AnovaOneWay")
{
}

Options *AnovaOneWay::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("variables"));
	options->add(new OptionField("factor"));

	return options;
}
