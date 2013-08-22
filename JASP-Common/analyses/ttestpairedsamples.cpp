#include "ttestpairedsamples.h"

#include "options.h"
#include "option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"
#include "options/optionfieldpairs.h"

#include "rinterface.h"

using namespace Json;
using namespace analyses;

TTestPairedSamples::TTestPairedSamples(int id)
	: Analysis(id, "TTestPairedSamples")
{
}

Options *TTestPairedSamples::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFieldPairs("pairs"));

	return options;
}
