
#include "analysis.h"

#include <QDebug>

#include <boost/bind.hpp>

#include "options.h"

#include <QStringBuilder>

#include <boost/foreach.hpp>

using namespace boost::uuids;
using namespace boost;

Analysis::Analysis(int id, string name, Options* options)
{
	_id = id;
	_name = name;
	_revision = 0;
	_inited = false;

	_options = options;
	_options->onChange.connect(boost::bind(&Analysis::optionsChangedHandler, this));

}

void Analysis::setResults(string results)
{
	_inited = true;
	_results = results;
}

string Analysis::results()
{
	typedef pair<string, AnalysisPart *> pair;

	Json::Value analysisAsJson = Json::Value(Json::objectValue);

	analysisAsJson["id"] = _id;
	analysisAsJson["name"] = _name;
	analysisAsJson["revision"] = _revision;

	Json::Value results = Json::Value(Json::objectValue);

	BOOST_FOREACH(pair p, _analysisParts)
	{
		string name = p.first;
		Json::Value partResults = p.second->results();

		results[name] = partResults;
	}

	analysisAsJson["analyses"] = results;

	return analysisAsJson.toStyledString();
}

int Analysis::revision()
{
	return _revision;
}

bool Analysis::isInitialised()
{
	return _inited;
}

string Analysis::name()
{
	return _name;
}

int Analysis::id()
{
	return _id;
}

void Analysis::initialise(Json::Value results)
{
	for (Json::ValueIterator itr = results.begin(); itr != results.end(); itr++)
	{
		string name = itr.key().asString();
		Json::Value result = results.get(name, Json::nullValue);

		if (_analysisParts.find(name) != _analysisParts.end())
		{
			AnalysisPart *part = _analysisParts.at(name);
			_analysisParts.erase(name);
			delete part;
		}

		if ( ! result.isNull())
		{
			_analysisParts[name] = new AnalysisPart(this, name, result);
		}

	}

	_inited = true;

	resultsChanged(this);
}

Options *Analysis::options()
{


	/*_options["mainFields"] = Value(arrayValue);
	_options["mainDisplayFrequencyTables"] = true;

	_options["statisticsPercentileValuesQuartiles"] = true;
	_options["statisticsPercentileValuesEqualGroups"] = true;
	_options["statisticsPercentileValuesEqualGroupsNumberOfGroups"] = 10;
	_options["statisticsPercentileValuesPercentiles"] = 10;
	_options["statisticsPercentileValuesPercentilesPercentiles"] = Value(arrayValue);

	_options["statisticsCentralTendencyMean"] = false;
	_options["statisticsCentralTendencyMedian"] = false;
	_options["statisticsCentralTendencyMode"] = false;
	_options["statisticsCentralTendencySum"] = false;

	_options["statisticsValuesAreGroupMeans"] = false;

	_options["statisticsDispersionStdDeviation"] = false;
	_options["statisticsDispersionVariance"] = false;
	_options["statisticsDispersionRange"] = false;
	_options["statisticsDispersionMinimum"] = false;
	_options["statisticsDispersionMaximum"] = false;
	_options["statisticsDispersionStdErrorMean"] = false;

	_options["statisticsDistributionSkewness"] = false;
	_options["statisticsDistributionKurtosis"] = false;*/

	return _options;
}

void Analysis::optionsChangedHandler()
{
	_revision++;

	/*BOOST_FOREACH(AnalysisPart *part, *this)
	{
		part->revise();
	}*/

	optionsChanged(this);
}


/*Analysis::iterator Analysis::begin()
{
	return _analysisParts->begin();
}

Analysis::iterator Analysis::end()
{
	return _analysisParts->end();
}

bool Analysis::isCompleted()
{
	BOOST_FOREACH(AnalysisPart* part, *this)
	{
		if ( ! part->isCompleted())
			return false;
	}

	return true;
}*/
