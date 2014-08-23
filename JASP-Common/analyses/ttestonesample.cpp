#include "ttestonesample.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"
#include "options/optionnumber.h"

using namespace std;

TTestOneSample::TTestOneSample(int id)
	: Analysis(id, "TTestOneSample", createOptions())
{
}

Options *TTestOneSample::createOptions() const
{
	Options *options = new Options();

	options->add("variables", new OptionVariables());
	options->add("testValue", new OptionNumber(0));

	options->add("hypothesis", new OptionList(list("notEqualToTestValue", "greaterThanTestValue", "lessThanTestValue")));

	options->add("meanDifference", new OptionBoolean());
	options->add("confidenceInterval", new OptionBoolean());
	options->add("confidenceIntervalInterval", new OptionNumber(.95, 0, 1, "%"));
	options->add("descriptives", new OptionBoolean());

	vector<string> missingValues;
	missingValues.push_back("excludeAnalysisByAnalysis");
	missingValues.push_back("excludeListwise");

	options->add("missingValues", new OptionList(missingValues));

	return options;
}
