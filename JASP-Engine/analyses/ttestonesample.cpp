#include "ttestonesample.h"

#include "boost/foreach.hpp"
#include "ttestonesample.R.h"

using namespace Json;

TTestOneSample::TTestOneSample(DataSet *dataSet, Json::Value *options, RInterface *r)
	: AnalysisTask(dataSet, options, r)
{
}

void TTestOneSample::init()
{
	Value results = Value(objectValue);

	Value ttest = objectValue;

	const char mu[] = { 0xCE, 0xBC, 0x0 } ;

	Value variables = arrayValue;
	variables.append(string(mu));
	variables.append("t");
	variables.append("df");
	variables.append("p");

	Value cases = arrayValue;

	BOOST_FOREACH(string field, getMainFields())
		cases.append(field);

	ttest["title"] = "One-Sample T-Test";
	ttest["subtitle"] = "Test Value = 0";
	ttest["testValue"] = 0;
	ttest["variables"] = variables;
	ttest["cases"] = cases;
	ttest["data"] = nullValue;

	results["ttest"] = ttest;

	_results.clear();
	_results.swap(results);
}

void TTestOneSample::run()
{
	string script((const char *)ttestonesample_R, ttestonesample_R_size);

	_r->setDataSet(_dataSet);
	_r->setOptions(*_options);
    _results["ttest"]["data"] = _r->run(script);
}

vector<string> TTestOneSample::getMainFields()
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
    //	qDebug() << "main/fields not found in OneSampleTTest!";

	return fields;
}
