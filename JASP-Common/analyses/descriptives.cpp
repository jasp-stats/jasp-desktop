#include "descriptives.h"

#include "analysis.h"

#include "options.h"
#include "option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"

using namespace std;

Descriptives::Descriptives(int id)
	: Analysis(id, "Descriptives")
{
}

Options *Descriptives::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("main/fields"));
	options->add(new OptionBoolean("main/displayFrequencyTables"));

	options->add(new OptionBoolean("statistics/percentileValues/quartiles"));
	options->add(new OptionBoolean("statistics/percentileValues/equalGroups"));
	options->add(new OptionBoolean("statistics/percentileValues/percentiles"));

	options->add(new OptionInteger("statistics/percentileValues/equalGroupsNo", 4));
	options->add(new OptionIntegerArray("statistics/percentileValues/percentilesPercentiles"));

	options->add(new OptionBoolean("statistics/centralTendency/mean", true));
	options->add(new OptionBoolean("statistics/centralTendency/median"));
	options->add(new OptionBoolean("statistics/centralTendency/mode"));
	options->add(new OptionBoolean("statistics/centralTendency/sum"));

	options->add(new OptionBoolean("statistics/valuesAreGroupMidpoints"));

	options->add(new OptionBoolean("statistics/dispersion/standardDeviation", true));
	options->add(new OptionBoolean("statistics/dispersion/variance"));
	options->add(new OptionBoolean("statistics/dispersion/range"));
	options->add(new OptionBoolean("statistics/dispersion/minimum", true));
	options->add(new OptionBoolean("statistics/dispersion/maximum", true));
	options->add(new OptionBoolean("statistics/dispersion/standardErrorMean"));

	options->add(new OptionBoolean("statistics/distribution/skewness"));
	options->add(new OptionBoolean("statistics/distribution/kurtosis"));

	vector<string> chartType;
	chartType.push_back("noCharts");
	chartType.push_back("barCharts");

	vector<string> chartValues;
	chartType.push_back("frequencies");
	chartType.push_back("percentages");

	options->add(new OptionList("charts/chartType", chartType));
	options->add(new OptionBoolean("charts/showNormalCurve"));
	options->add(new OptionList("charts/chartValues", chartValues));

	return options;
}
