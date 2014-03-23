
#include "ttestbayesianonesample.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"

using namespace std;

TTestBayesianOneSample::TTestBayesianOneSample(int id)
	: Analysis(id, "TTestBayesianOneSample")
{
}

Options *TTestBayesianOneSample::createDefaultOptions()
{
	Options *options = new Options();

	options->add("variables", new OptionFields());
	options->add("missingValues", new OptionList(list("excludeAnalysisByAnalysis", "excludeListwise")));

	return options;
}
