
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
}

Analysis::~Analysis()
{
	delete _options;
}

void Analysis::abort()
{
	_status = Aborting;
	optionsChanged(this);
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

const Json::Value &Analysis::results() const
{
	return _results;
}

Json::Value Analysis::asJSON() const
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

Analysis::Status Analysis::status() const
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

