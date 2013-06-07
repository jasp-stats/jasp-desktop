#include "analyses.h"

#include "analysisloader.h"

Analyses::Analyses()
{
	_nextId = 0;
}

Analysis *Analyses::create(string name)
{
	int id = _nextId++;

	Analysis *analysis = AnalysisLoader::load(id, name);
	_analyses[id] = analysis;

	analysis->optionsChanged.connect(boost::bind(&Analyses::analysisOptionsChangedHandler, this, _1));
	analysis->resultsChanged.connect(boost::bind(&Analyses::analysisResultsChangedHandler, this, _1));

	analysisAdded(analysis);

	return analysis;
}

Analysis *Analyses::get(int id)
{
	return _analyses.at(id);
}

void Analyses::analysisResultsChangedHandler(Analysis *analysis)
{
	analysisResultsChanged(analysis);
}


void Analyses::analysisOptionsChangedHandler(Analysis *analysis)
{
	analysisOptionsChanged(analysis);
}



