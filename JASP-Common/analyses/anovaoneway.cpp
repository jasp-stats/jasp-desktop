
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

using namespace std;

AnovaOneWay::AnovaOneWay(int id)
	: Analysis(id, "AnovaOneWay")
{
}

Options *AnovaOneWay::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("variables"));
	options->add(new OptionField("groupingVariable"));

	vector<string> equalityOfVariances;
	equalityOfVariances.push_back("assumeEqual");
	equalityOfVariances.push_back("assumeUnequal");
	equalityOfVariances.push_back("both");

	options->add(new OptionList("equalityOfVariances", equalityOfVariances));

	options->add(new OptionBoolean("testUnequalVariances"));

	options->add(new OptionBoolean("meanDifference"));
	options->add(new OptionBoolean("confidenceInterval"));
	options->add(new OptionNumber("confidenceIntervalInterval", .95, 0, 1, "%"));
	options->add(new OptionBoolean("descriptives"));

	vector<string> missingValues;
	missingValues.push_back("excludeAnalysisByAnalysis");
	missingValues.push_back("excludeListwise");

	options->add(new OptionList("missingValues", missingValues));

	vector<string> tails;
	tails.push_back("twoTailed");
	tails.push_back("oneTailedGreaterThan");
	tails.push_back("oneTailedLessThan");

	return options;
}
