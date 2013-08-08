#include "ttestindependentsamples.h"

#include "boost/foreach.hpp"
#include "ttestindependentsamples.R.h"

#include "options.h"
#include "option.h"
#include "options/optionfield.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"

#include "rinterface.h"

using namespace Json;
using namespace analyses;

TTestIndependentSamples::TTestIndependentSamples(int id)
	: Analysis(id, "TTestIndependentSamples")
{
}

/*void TTestIndependentSamples::init()
{
	_r->setDataSet(_dataSet);
	_r->setOptions(_options->asJSON());
	_results = _r->run(createScript(), "init");
}

void TTestIndependentSamples::run()
{
	_r->setDataSet(_dataSet);
	_r->setOptions(_options->asJSON());
	_results = _r->run(createScript(), "run");
}*/

string TTestIndependentSamples::_script((const char *)ttestindependentsamples_R, ttestindependentsamples_R_size);

string TTestIndependentSamples::rScript()
{
	return _script;
}

Options *TTestIndependentSamples::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("variables"));
	options->add(new OptionField("groupingVariable"));
	options->add(new OptionList("equalityOfVariances", "assumeEqual"));
	options->add(new OptionBoolean("testUnequalVariances"));

	options->add(new OptionBoolean("meanDifference"));
	options->add(new OptionBoolean("confidenceInterval"));
	options->add(new OptionNumber("confidenceIntervalInterval", .95, 0, 1, 3));
	options->add(new OptionBoolean("descriptives"));

	options->add(new OptionList("missingValues", "excludeAnalysisByAnalysis"));

	return options;
}
