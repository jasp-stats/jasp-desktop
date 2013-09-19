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
    options->add(new OptionList("tails", "twoTailed"));

    options->add(new OptionBoolean("meanDifference"));
    options->add(new OptionBoolean("confidenceInterval"));
    options->add(new OptionNumber("confidenceIntervalInterval", .95, 0, 1, 3));
    options->add(new OptionBoolean("descriptives"));
    options->add(new OptionBoolean("effectSize"));


    options->add(new OptionList("missingValues", "excludeAnalysisByAnalysis"));

	return options;
}
