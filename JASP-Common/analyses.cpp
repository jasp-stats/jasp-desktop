#include "analyses.h"

#include "analysisloader.h"
#include "boost/foreach.hpp"

using namespace std;

Analyses::Analyses()
{
	_nextId = 0;
}

Analysis *Analyses::create(string name, Json::Value *optionsData, Analysis::Status status)
{
	return create(name, _nextId++, optionsData, status);
}

Analysis *Analyses::create(string name, int id, Json::Value *optionsData, Analysis::Status status)
{
	if (id >= _nextId)
		_nextId = id + 1;

	Analysis *analysis = AnalysisLoader::load(id, name, optionsData, status);

	if (optionsData == NULL && _defaults.find(name) != _defaults.end())
	{
		Json::Value opt = _defaults[name]->options()->asJSON();
		analysis->options()->set(opt);
	}

	while (id >= _analyses.size())
		_analyses.push_back(NULL);

	_analyses[id] = analysis;

	analysis->optionsChanged.connect(boost::bind(&Analyses::analysisOptionsChangedHandler, this, _1));
	analysis->resultsChanged.connect(boost::bind(&Analyses::analysisResultsChangedHandler, this, _1));

	analysisAdded(analysis);

	return analysis;
}

void Analyses::clear()
{
	for (Analyses::iterator itr = this->begin(); itr != this->end(); itr++)
	{
		Analysis *analysis = *itr;
		if (analysis->status() != Analysis::Complete)
			analysis->setStatus(Analysis::Aborted);
	}
}

Analysis *Analyses::get(int id)
{
	if (id < _analyses.size())
		return _analyses.at(id);
	else
		return NULL;
}

int Analyses::size()
{
	return _analyses.size();
}

std::vector<Analysis*>::iterator Analyses::begin()
{
	return _analyses.begin();
}

std::vector<Analysis*>::iterator Analyses::end()
{
	return _analyses.end();
}

void Analyses::analysisResultsChangedHandler(Analysis *analysis)
{
	analysisResultsChanged(analysis);
}


void Analyses::analysisOptionsChangedHandler(Analysis *analysis)
{
	_defaults[analysis->name()] = analysis;

	analysisOptionsChanged(analysis);
}





