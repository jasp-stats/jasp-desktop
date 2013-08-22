
#include "analysis.h"

#include <QDebug>

#include <boost/bind.hpp>

#include "options.h"

#include <QStringBuilder>

#include <boost/foreach.hpp>

using namespace boost::uuids;
using namespace boost;
using namespace std;

Analysis::Analysis(int id, string name)
{
	_id = id;
	_name = name;

	_revision = 0;
	_inited = false;
	_dataSet = NULL;
	_r = NULL;

	_options = NULL;

}

void Analysis::init()
{
	_r->setDataSet(_dataSet);
	_results = _r->init(_id, _name, _options->asJSON());
}

void Analysis::run()
{
	_r->setDataSet(_dataSet);
	_results = _r->run(_id, _options->asJSON());
}

void Analysis::setResults(Json::Value results)
{
	_inited = true;
	_results = results;

	resultsChanged(this);
}

Json::Value Analysis::results()
{
	/*Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"] = _id;
	analysisAsJson["name"] = _name;
	analysisAsJson["revision"] = _revision;

	analysisAsJson["results"] = _results;

	return analysisAsJson;*/

	return _results;
}

Json::Value Analysis::asJSON()
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"] = _id;
	analysisAsJson["name"] = _name;
	analysisAsJson["revision"] = _revision;
	analysisAsJson["results"] = _results;

	return analysisAsJson;
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

Options *Analysis::options()
{
	if (_options == NULL)
	{
		_options = createDefaultOptions();
		_options->onChange.connect(boost::bind(&Analysis::optionsChangedHandler, this));
	}

	return _options;
}

void Analysis::optionsChangedHandler()
{
	_revision++;

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


void Analysis::setRInterface(RInterface *r)
{
	_r = r;
}

void Analysis::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;
}

