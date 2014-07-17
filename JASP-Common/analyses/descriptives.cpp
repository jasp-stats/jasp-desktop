#include "descriptives.h"

#include "analysis.h"

#include "options/options.h"
#include "options/option.h"
#include "options/optionvariables.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"

using namespace std;

Descriptives::Descriptives(int id)
	: Analysis(id, "Descriptives", createOptions())
{
}

Options *Descriptives::createOptions() const
{
	Options *options = new Options();

	options->add("main/fields", new OptionVariables());
	options->add("main/displayFrequencyTables", new OptionBoolean());

	options->add("statistics/percentileValues/quartiles", new OptionBoolean());
	options->add("statistics/percentileValues/equalGroups", new OptionBoolean());
	options->add("statistics/percentileValues/percentiles", new OptionBoolean());

	options->add("statistics/percentileValues/equalGroupsNo", new OptionInteger(4));
	options->add("statistics/percentileValues/percentilesPercentiles", new OptionIntegerArray());

	options->add("statistics/centralTendency/mean", new OptionBoolean(true));
	options->add("statistics/centralTendency/median", new OptionBoolean());
	options->add("statistics/centralTendency/mode", new OptionBoolean());
	options->add("statistics/centralTendency/sum", new OptionBoolean());

	options->add("statistics/valuesAreGroupMidpoints", new OptionBoolean());

	options->add("statistics/dispersion/standardDeviation", new OptionBoolean(true));
	options->add("statistics/dispersion/variance", new OptionBoolean());
	options->add("statistics/dispersion/range", new OptionBoolean());
	options->add("statistics/dispersion/minimum", new OptionBoolean(true));
	options->add("statistics/dispersion/maximum", new OptionBoolean(true));
	options->add("statistics/dispersion/standardErrorMean", new OptionBoolean());

	options->add("statistics/distribution/skewness", new OptionBoolean());
	options->add("statistics/distribution/kurtosis", new OptionBoolean());

	options->add("charts/chartType", new OptionList(list("noCharts", "barCharts", "barCharts", "barCharts")));
	options->add("charts/showNormalCurve", new OptionBoolean());
	options->add("charts/chartValues", new OptionList(list("frequencies", "percentages")));
	options->add("chartWidth", new OptionInteger(480));
	options->add("chartHeight", new OptionInteger(320));

	return options;
}
