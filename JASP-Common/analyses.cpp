#include "analyses.h"

#include "analysisloader.h"
#include "boost/foreach.hpp"

Analyses::Analyses()
{
	_nextId = 0;
}

Analysis *Analyses::create(string name)
{
	return create(name, _nextId++);
}

Analysis *Analyses::create(string name, int id)
{
	Analysis *analysis = AnalysisLoader::load(id, name);

	if (_defaults.find(name) != _defaults.end())
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

Analysis *Analyses::get(int id)
{
	if (id < _analyses.size())
		return _analyses.at(id);
	else
		return NULL;
}

/*Analysis *Analyses::next()
{
	static int j = 0;

	Analysis *next = NULL;

	for (ById::iterator itr = _analyses.begin(); itr != _analyses.end(); itr++)
	{
		Analysis *analysis = itr->second;

		if (analysis->isCompleted() == false && analysis->isInitialised() == false)
			return analysis;

		if (next == NULL && analysis->isCompleted() == false)
			next = analysis;
	}

	return next;
}*/



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





