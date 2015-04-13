#ifndef ANALYSES_H
#define ANALYSES_H

#include <boost/signals2.hpp>
#include <boost/bind.hpp>
#include <map>

#include "analysis.h"

class Analyses
{
	friend class EngineSync;
	friend class boost::iterator_core_access;

	typedef std::map<int, Analysis *> ById;

public:
	Analyses();

    Analysis *create(std::string name, Json::Value *optionsData = NULL, Analysis::Status status = Analysis::Empty);
    Analysis *create(std::string name, int id, Json::Value *optionsData = NULL, Analysis::Status status = Analysis::Empty);

	Analysis *get(int id);
	void clear();
	int size();

	boost::signals2::signal<void (Analysis *)> analysisInitialised;
	boost::signals2::signal<void (Analysis *)> analysisOptionsChanged;
	boost::signals2::signal<void (Analysis *)> analysisResultsChanged;
	boost::signals2::signal<void (Analysis *)> analysisAdded;

	typedef std::vector<Analysis*>::iterator iterator;
	iterator begin();
	iterator end();

private:

	void analysisOptionsChangedHandler(Analysis *analysis);
	void analysisResultsChangedHandler(Analysis *analysis);

	std::vector<Analysis*> _analyses;

	int _nextId;

	std::map<std::string, Analysis *> _defaults;



};


#endif // ANALYSES_H
