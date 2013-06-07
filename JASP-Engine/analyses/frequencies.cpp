#include "frequencies.h"

#include <iomanip>

#include <boost/foreach.hpp>

#include "analysistask.h"
#include "frequencies.R.h"

using namespace Json;

Frequencies::Frequencies(DataSet *dataSet, Value *options, RInterface *r)
	: AnalysisTask(dataSet, options, r)
{
}

void Frequencies::init()
{
	Value analyses = Value(objectValue);

	analyses["stats"] = initStats();
	analyses["tables"] = initTables();
	analyses["plots"] = initPlots();

	_results.clear();
	_results.swap(analyses);
}

void Frequencies::run()
{
	vector<string> fields = getMainFields();

	int i = 0;

	Value frequencyTables = _results.get("tables", arrayValue);
	Value frequencyPlots = _results.get("plots", arrayValue);

	Columns &columns = _dataSet->columns();

	BOOST_FOREACH(string field, fields)
	{
		Column *column = columns.get(field);
		(void)column;

		if (column->columnType() == Column::DoubleColumnType)
			continue;

		map<int, int> cases;

		BOOST_FOREACH(int value, column->AsInts)
		{
			int count = cases[value];
			count++;
			cases[value] = count;
		}

		Value frequencyTable = frequencyTables[i];
		Value frequencyPlot = frequencyPlots[i];

		frequencyTable["data"] = Value(arrayValue);

		int caseNo = 0;
		int total = 0;

		std::pair<int, int> p;

		BOOST_FOREACH(p, cases)
		{
			total += p.second;

			string label = column->displayFromValue(p.first);
			if (label == "")
				label = "Missing";

			frequencyTable["cases"][caseNo] = label;

			frequencyTable["data"][caseNo] = arrayValue;
			frequencyTable["data"][caseNo].append(p.second);  // frequency

			frequencyPlot["cases"].append(label);
			frequencyPlot["data"].append(p.second);

			caseNo++;
		}

		caseNo = 0;
		int cum = 0;

		BOOST_FOREACH(p, cases)
		{
			cum += p.second;

			double percent = (double)p.second / total * 100;
			double validPercent = percent;
			double cumPercent = (double)cum / total * 100;

			frequencyTable["data"][caseNo].append(percent);
			frequencyTable["data"][caseNo].append(validPercent);
			frequencyTable["data"][caseNo].append(cumPercent);

			caseNo++;
		}

		frequencyTable["cases"][caseNo] = Value("Total");
		frequencyTable["data"][caseNo].append(total);
		frequencyTable["data"][caseNo].append(100.0);
		frequencyTable["data"][caseNo].append(100.0);
		frequencyTable["data"][caseNo].append("");

		frequencyTables[i] = frequencyTable;
		frequencyPlots[i] = frequencyPlot;

		i++;
	}

	_results["tables"] = frequencyTables;
	_results["plots"] = frequencyPlots;

    string script((const char *)frequencies_R, frequencies_R_size);

    cout << script;
    cout.flush();

	_r->setDataSet(_dataSet);
	_r->setOptions(*_options);
    _results["stats"]["data"] = _r->run(script);
}

Json::Value Frequencies::initTables()
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

	Value nValid(arrayValue);
	nValid.append("N");
	nValid.append("Valid");

	Value nMissing(arrayValue);
	nMissing.append("");
	nMissing.append("Missing");

	variables.append(nValid);
	variables.append(nMissing);

	if (statsOptions["centralTendency"]["mean"].asBool())
		variables.append("Mean");
	if (statsOptions["centralTendency"]["median"].asBool())
		variables.append("Median");
	if (statsOptions["centralTendency"]["mode"].asBool())
		variables.append("Mode");
	if (statsOptions["centralTendency"]["sum"].asBool())
		variables.append("Sum");

	BOOST_FOREACH(string field, fields)
	{

	}

	statsResults["variables"] = variables;
	statsResults["cases"] = cases;
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
}
