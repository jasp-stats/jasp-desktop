
#include "analysis.h"

#include <QDebug>

#include <boost/bind.hpp>

#include "options/options.h"

#include <QStringBuilder>

#include <boost/foreach.hpp>

using namespace boost::uuids;
using namespace boost;
using namespace std;

Analysis::Analysis(int id, string name)
{
	_id = id;
	_name = name;

	_status = Empty;

	_dataSet = NULL;
	_r = NULL;

	_options = NULL;
}

void Analysis::init()
{
	_status = Initing;

	_results = _r->init(_name, options()->asJSON());

	_status = Inited;
	resultsChanged(this);
}

void Analysis::run()
{
	_status = Running;
	_results = _r->run(_name, options()->asJSON(), boost::bind(&Analysis::callback, this, _1));

	// status can be changed by subsequent messages, so we have to see if the analysis has
	// changed. if it has, then we shouldn't bother sending the results

	if (_status == Running)
	{
		_status = Complete;
		resultsChanged(this);
	}
}

void Analysis::setResults(Json::Value results)
{
	_results = results;
	resultsChanged(this);
}

Json::Value Analysis::results()
{
	return _results;
}

Json::Value Analysis::asJSON()
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"] = _id;
	analysisAsJson["name"] = _name;
	analysisAsJson["results"] = _results;

	return analysisAsJson;
}

Analysis::Status Analysis::status()
{
	return _status;
}

void Analysis::setStatus(Analysis::Status status)
{
	_status = status;
}

string Analysis::name()
{
	return _name;
}

int Analysis::id()
{
	return _id;
}

Options *Analysis::options()
{
	if (_options == NULL)
	{
		_options = createDefaultOptions();
		_options->changed.connect(boost::bind(&Analysis::optionsChangedHandler, this));
	}

	return _options;
}

void Analysis::optionsChangedHandler()
{
	_status = Empty;
	optionsChanged(this);
}

void Analysis::setRInterface(RInterface *r)
{
	_r = r;
}

void Analysis::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;
}

int Analysis::callback(Json::Value results)
{
	if (_status != Empty && _status != Aborted)
	{
		if (results != Json::nullValue)
		{
			_results = results;
			resultsChanged(this);
		}
		return 0;
	}
	else
	{
		return 1;
	}
}
