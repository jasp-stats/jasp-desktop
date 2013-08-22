#include "anovamultivariate.h"

#include "options.h"
#include "option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"

#include "rinterface.h"

using namespace Json;
using namespace analyses;

AnovaMultivariate::AnovaMultivariate(int id)
	: Analysis(id, "AnovaMultivariate")
{
}

Options *AnovaMultivariate::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("variables"));

	return options;
}
