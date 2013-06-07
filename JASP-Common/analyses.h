#ifndef ANALYSES_H
#define ANALYSES_H

#include <boost/signals2.hpp>
#include <boost/bind.hpp>
#include <map>

#include "analysis.h"

class Analyses
{
	friend class EngineSync;


public:
	Analyses();

	Analysis *create(string name);
	Analysis *get(int id);

	boost::signals2::signal<void (Analysis *)> analysisInitialised;
	boost::signals2::signal<void (Analysis *)> analysisOptionsChanged;
	boost::signals2::signal<void (Analysis *)> analysisResultsChanged;
	boost::signals2::signal<void (Analysis *)> analysisAdded;

private:

	void analysisOptionsChangedHandler(Analysis *analysis);
	void analysisResultsChangedHandler(Analysis *analysis);

	typedef map<int, Analysis *> ById;

	ById _analyses;



	int _nextId;

};

#endif // ANALYSES_H
