#include "frequencies.h"

#include <iomanip>

#include <boost/foreach.hpp>

#include "analysis.h"
#include "frequencies.R.h"

#include "options.h"
#include "option.h"
#include "options/optionfields.h"
#include "options/optionboolean.h"
#include "options/optioninteger.h"
#include "options/optionintegerarray.h"
#include "options/optionlist.h"

using namespace Json;
using namespace analyses;

Frequencies::Frequencies(int id)
	: Analysis(id, "Descriptives")
{
}

string Frequencies::_script((const char *)frequencies_R, frequencies_R_size);

void Frequencies::init()
{
	_r->setDataSet(_dataSet);
	_r->setOptions(_options->asJSON());
	_results = _r->run(_script, "init");
}

void Frequencies::run()
{
	_r->setDataSet(_dataSet);
	_r->setOptions(_options->asJSON());
	_results = _r->run(_script, "run");
}

Options *Frequencies::createDefaultOptions()
{
	Options *options = new Options();

	options->add(new OptionFields("main/fields"));
	options->add(new OptionBoolean("main/displayFrequencyTables"));

	options->add(new OptionBoolean("statistics/percentileValues/quartiles"));
	options->add(new OptionBoolean("statistics/percentileValues/equalGroups"));
	options->add(new OptionBoolean("statistics/percentileValues/percentiles"));

	options->add(new OptionInteger("statistics/percentileValues/equalGroupsNo", 4));
	options->add(new OptionIntegerArray("statistics/percentileValues/percentilesPercentiles"));

	options->add(new OptionBoolean("statistics/centralTendency/mean"));
	options->add(new OptionBoolean("statistics/centralTendency/median"));
	options->add(new OptionBoolean("statistics/centralTendency/mode"));
	options->add(new OptionBoolean("statistics/centralTendency/sum"));

	options->add(new OptionBoolean("statistics/valuesAreGroupMidpoints"));

	options->add(new OptionBoolean("statistics/dispersion/standardDeviation"));
	options->add(new OptionBoolean("statistics/dispersion/variance"));
	options->add(new OptionBoolean("statistics/dispersion/range"));
	options->add(new OptionBoolean("statistics/dispersion/minimum"));
	options->add(new OptionBoolean("statistics/dispersion/maximum"));
	options->add(new OptionBoolean("statistics/dispersion/standardErrorMean"));

	options->add(new OptionBoolean("statistics/distribution/skewness"));
	options->add(new OptionBoolean("statistics/distribution/kurtosis"));

	options->add(new OptionList("charts/chartType", "noCharts"));
	options->add(new OptionBoolean("charts/showNormalCurve"));
	options->add(new OptionList("charts/chartValues", "frequencies"));

	return options;
}

/*Json::Value Frequencies::initTables()
{
	Value frequencyTables = Value(arrayValue);

	vector<string> fields = getMainFields();
	const char* columnNames[] = { "Frequency", "Percent", "Valid Percent", "Cumulative Percent" };

	Columns &columns = _dataSet->columns();

	BOOST_FOREACH(string field, fields)
	{
		Column *column = columns.get(field);

		if (column->columnType() == Column::DoubleColumnType)
			continue;

		Value frequencyTable(objectValue);
		frequencyTable["title"] = field;

		Value cases(arrayValue);
		cases.append(Value("Total"));
		frequencyTable["cases"] = cases;

		Value variables(arrayValue);
		variables.append(Value(columnNames[0]));
		variables.append(Value(columnNames[1]));
		variables.append(Value(columnNames[2]));
		variables.append(Value(columnNames[3]));

		frequencyTable["variables"] = variables;

		Value frequency = objectValue;
		frequency["id"] = "Frequency";
		frequency["type"] = "integer";

		Value percent = objectValue;
		percent["id"] = "Percent";
		percent["type"] = "number";
		percent["format"] = "dp:1";

		Value validPercent = objectValue;
		validPercent["id"] = "Valid Percent";
		validPercent["type"] = "number";
		validPercent["format"] = "dp:1";

		Value cumPercent = objectValue;
		cumPercent["id"] = "Cumulative Percent";
		cumPercent["type"] = "number";
		cumPercent["format"] = "dp:1";

		Value fieldsValue = arrayValue;
		fieldsValue.append(frequency);
		fieldsValue.append(percent);
		fieldsValue.append(validPercent);
		fieldsValue.append(cumPercent);

		Value schema = objectValue;
		schema["fields"] = fieldsValue;

		frequencyTable["schema"] = schema;

		frequencyTable["data"] = Value(nullValue);

		frequencyTables.append(frequencyTable);
	}

	return frequencyTables;
}

Value Frequencies::initStats()
{
	Value statsOptions = _options->get("statistics", objectValue);
	Value statsResults = Value(objectValue);

	vector<string> fields = getMainFields();

	Value variables(arrayValue);
	Value cases = _options->get("main", objectValue)["fields"];
	Value schema = objectValue;

	schema["fields"] = arrayValue;

	variables.append("Valid");
	variables.append("Missing");

	Value field = objectValue;
	field["id"] = "Valid";
	field["type"] = "integer";

	schema["fields"].append(field);

	field = objectValue;
	field["id"] = "Missing";
	field["type"] = "integer";

	schema["fields"].append(field);

	if (statsOptions["centralTendency"]["mean"].asBool())
	{
		variables.append("Mean");

		field = objectValue;
		field["id"] = "Mean";
		field["type"] = "number";
		field["format"] = "sf:4";

		schema["fields"].append(field);
	}

	if (statsOptions["centralTendency"]["median"].asBool())
	{
		variables.append("Median");

		field = objectValue;
		field["id"] = "Median";
		field["type"] = "number";
		field["format"] = "sf:4";

		schema["fields"].append(field);
	}

	if (statsOptions["centralTendency"]["mode"].asBool())
	{
		variables.append("Mode");

		field = objectValue;
		field["id"] = "Mode";
		field["type"] = "number";
		field["format"] = "sf:4";

		schema["fields"].append(field);
	}

	if (statsOptions["centralTendency"]["sum"].asBool())
	{
		variables.append("Sum");

		field = objectValue;
		field["id"] = "Sum";
		field["type"] = "number";
		field["format"] = "sf:4";

		schema["fields"].append(field);
	}

	BOOST_FOREACH(string field, fields)
	{

	}

	statsResults["variables"] = variables;
	statsResults["cases"] = cases;
	statsResults["schema"] = schema;
	statsResults["data"] = Json::nullValue;

	return statsResults;
}

Value Frequencies::initPlots()
{
	Value frequencyPlots = arrayValue;

	vector<string> fields = getMainFields();
	Columns &columns = _dataSet->columns();

	BOOST_FOREACH(string field, fields)
	{
		Column *column = columns.get(field);

		if (column->columnType() == Column::DoubleColumnType)
			continue;

		Value frequencyPlot(objectValue);

		frequencyPlot["title"] = field;
		frequencyPlot["cases"] = arrayValue;
		frequencyPlot["data"] = arrayValue;

		frequencyPlots.append(frequencyPlot);
	}

	return frequencyPlots;
}

vector<string> Frequencies::getMainFields()
{
	vector<string> fields;

	Value v = _options->get("main", objectValue).get("fields", nullValue);

	if (v.type() == arrayValue)
	{
		for (Value::iterator itr = v.begin(); itr != v.end(); itr++)
		{
			Value fieldName = *itr;
			if (fieldName.isString())
				fields.push_back(fieldName.asString());
		}
	}
    //else
    //	qDebug() << "main/fields not found in frequencies!";

	return fields;
}*/
