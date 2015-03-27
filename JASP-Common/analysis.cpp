
#include "analysis.h"

#include <QDebug>

#include <boost/bind.hpp>

#include "options/options.h"

#include <QStringBuilder>

#include <boost/foreach.hpp>

using namespace boost::uuids;
using namespace boost;
using namespace std;

Analysis::Analysis(int id, string name, Options *options, bool autorun)
{
	_id = id;
	_name = name;
	_options = options;
	_autorun = autorun;

	_options->changed.connect(boost::bind(&Analysis::optionsChangedHandler, this, _1));

	_status = Empty;

	_dataSet = NULL;
	_r = NULL;
}

Analysis::~Analysis()
{
	delete _options;
}

void Analysis::init()
{
	_status = Initing;

	Json::Value returned = _r->init(_name, options()->asJSON());

	string status = returned.get("status", "").asString();

	if (status != "")
	{
		if (status == "complete")
			_status = Complete;
		else
			_status = Inited;

		_results = returned.get("results", Json::nullValue);
	}
	else
	{
		_results = returned;
		_status = Inited;
	}

	resultsChanged(this);
}

void Analysis::run()
{
	_status = Running;

	Json::Value returned = _r->run(_name, options()->asJSON(), boost::bind(&Analysis::callback, this, _1));

	// status can be changed by subsequent messages, so we have to see if the analysis has
	// changed. if it has, then we shouldn't bother sending the results

	if (_status == Running)
	{
		Json::Value status = returned.get("status", Json::nullValue);

		if (status != Json::nullValue)
			_results = returned.get("results", Json::nullValue);
		else
			_results = returned;

		_status = Complete;
		resultsChanged(this);
	}
}

void Analysis::abort()
{
	_status = Aborted;
}

void Analysis::scheduleRun()
{
	if (_autorun)
		return;

	setStatus(Inited);
	optionsChanged(this);
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

	string status;

	switch (_status)
	{
	case Analysis::Empty:
		status = "empty";
		break;
	case Analysis::Inited:
		status = "waiting";
		break;
	case Analysis::Running:
		status = "running";
		break;
	case Analysis::Complete:
		status = "complete";
		break;
	case Analysis::Aborted:
		status = "aborted";
		break;
	default:
		status = "error";
		break;
	}

	analysisAsJson["status"] = status;

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

const string &Analysis::name() const
{
	return _name;
}

int Analysis::id() const
{
	return _id;
}

bool Analysis::isAutorun() const
{
	return _autorun;
}

Options *Analysis::options() const
{
	return _options;
}

void Analysis::optionsChangedHandler(Option *option)
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

