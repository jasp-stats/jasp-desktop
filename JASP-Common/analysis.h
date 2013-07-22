#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <boost/uuid/uuid.hpp>

#include <map>
#include <list>

#include "dataset.h"
#include "options.h"

#include "analysispart.h"

#include "rinterface.h"

class Analysis
{

public:
	Analysis(int id, string name);

	Options *options();

	boost::signals2::signal<void (Analysis *source)> optionsChanged;
	boost::signals2::signal<void (Analysis *source)> resultsChanged;

	typedef vector<AnalysisPart*>::iterator iterator;

	//iterator begin();
	//iterator end();

	//bool isCompleted();

	void setResults(Json::Value results);
	Json::Value results();
	Json::Value asJSON();

	int revision();

	bool isInitialised();

	std::string name();
	int id();

	virtual void init() = 0;
	virtual void run() = 0;

	void setRInterface(RInterface *r);

	void setDataSet(DataSet *dataSet);

protected:

	virtual Options *createDefaultOptions() = 0;

	Options* _options;
	DataSet *_dataSet;

	RInterface *_r;
	Json::Value _results;

private:
	int _id;
	std::string _name;
	int _revision;


	std::map<std::string, AnalysisPart *> _analysisParts;

	void optionsChangedHandler();

	bool _inited;

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
