#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <QObject>

#include <boost/uuid/uuid.hpp>

#include <map>
#include <list>

#include "dataset.h"
#include "options.h"

#include "analysispart.h"

class Analysis
{

public:
	Analysis(int id, string name, Options *options);

	Options *options();

	boost::signals2::signal<void (Analysis *source)> optionsChanged;
	boost::signals2::signal<void (Analysis *source)> resultsChanged;

	typedef vector<AnalysisPart*>::iterator iterator;

	//iterator begin();
	//iterator end();

	//bool isCompleted();

	void setResults(std::string results);
	std::string results();

	int revision();

	bool isInitialised();

	std::string name();
	int id();

	void initialise(Json::Value results);

private:
	int _id;
	std::string _name;
	int _revision;

	Options* _options;
	std::map<std::string, AnalysisPart *> _analysisParts;

	void optionsChangedHandler();

	bool _inited;
	std::string _results;

	Json::Value _data;
};

/*namespace boost
{
	// specialize range_mutable_iterator and range_const_iterator in namespace boost
	template <>
	struct range_const_iterator< Analysis >
	{
		typedef Analysis::iterator type;
	};

	template<>
	struct range_mutable_iterator< Analysis >
	{
		typedef Analysis::iterator type;
	};
}*/


#endif // ANALYSIS_H
