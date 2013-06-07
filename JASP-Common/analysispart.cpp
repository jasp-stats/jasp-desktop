
#include "analysis.h"

using namespace std;

AnalysisPart::AnalysisPart(Analysis *analysis, string partName, Json::Value results)
{
	_parent = analysis;
	_name = partName;
	_results = results;

	_revision = 0;
	_completed = false;

}

bool AnalysisPart::isCompleted() const
{
	return _completed;
}

void AnalysisPart::setCompleted(bool value)
{
	_completed = value;
	completed(this);
}

string AnalysisPart::name()
{
	return _name;
}

Json::Value AnalysisPart::asJSON()
{
	Json::Value analysisPartAsJson = Json::Value(Json::objectValue);
	analysisPartAsJson["name"] = _name;
	analysisPartAsJson["options"] = _parent->options()->asJSON();

	return analysisPartAsJson;
}

Json::Value AnalysisPart::results() const
{
	return _results;
}

void AnalysisPart::setResults(const Json::Value results)
{
	_results = results;
}

int AnalysisPart::revision()
{
	return _revision;
}
