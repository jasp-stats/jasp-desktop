#include "descriptives.h"

#include "analysis.h"

#include "options/options.h"
#include "options/option.h"
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

	options->add("main/fields", new OptionFields());
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

	vector<string> chartType;
	chartType.push_back("noCharts");
	chartType.push_back("barCharts");
	chartType.push_back("barCharts");
	chartType.push_back("barCharts");

	vector<string> chartValues;
	chartValues.push_back("frequencies");
	chartValues.push_back("percentages");

	options->add("charts/chartType", new OptionList(chartType));
	options->add("charts/showNormalCurve", new OptionBoolean());
	options->add("charts/chartValues", new OptionList(chartValues));

	return options;
}

string Descriptives::js()
{
	return "{"
			"    depends : ['descriptives'], "
			"    render : function(element, results, status) { element.descriptives( { stats : results.stats, tables : results.tables, status : status } ) } "
			"}";
}
