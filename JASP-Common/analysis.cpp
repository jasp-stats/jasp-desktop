
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

Analysis::~Analysis()
{
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

string Analysis::js()
{
	return "{"
			"    depends : [ 'tables' ],\n"
			"    render  : function(element, results, status)\n"
			"    {\n"
			"        var tables = [ ]\n"
			"        var tableNames = \"" + this->order() + "\".split(',')\n"
			"        if (tableNames[0] !== \"\") {\n"
			"            _.each(tableNames, function(tableName) {\n"
			"                if (_.has(results, tableName))\n"
			"                    tables.push(results[tableName])\n"
			"            })\n"
			"        }\n"
			"        else {\n"
			"            _.each(results, function(result) {\n"
			"                if (_.isArray(result))\n"
			"                    _.each(result, function(table) {\n"
			"                        tables.push(table) })\n"
			"                else\n"
			"                    tables.push(result)\n"
			"            })\n"
			"        }\n"
			"        element.tables( { items : tables, status : status } )\n"
			"    }\n"
			"}\n";
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
		_options->changed.connect(boost::bind(&Analysis::optionsChangedHandler, this, _1));
	}

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

std::vector<string> Analysis::list(string one, string two, string three, string four, string five, string six, string seven, string eight, string nine, string ten)
{
	vector<string> result;

	result.push_back(one);
	result.push_back(two);

	if (three != "")
		result.push_back(three);
	if (four != "")
		result.push_back(four);
	if (five != "")
		result.push_back(five);
	if (six != "")
		result.push_back(six);
	if (seven != "")
		result.push_back(seven);
	if (eight != "")
		result.push_back(eight);
	if (nine != "")
		result.push_back(nine);
	if (ten != "")
		result.push_back(ten);

	return result;
}
