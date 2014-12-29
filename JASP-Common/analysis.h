#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <boost/uuid/uuid.hpp>

#include <map>
#include <list>

#include "dataset.h"
#include "options/options.h"

#include "rinterface.h"

#include "common.h"

class Analysis
{

public:
	Analysis(int id, std::string name, Options *options);
	virtual ~Analysis();

	Options *options() const;

	boost::signals2::signal<void (Analysis *source)> optionsChanged;
	boost::signals2::signal<void (Analysis *source)> resultsChanged;

	void setResults(Json::Value results);
	Json::Value results();
	Json::Value asJSON();

	const std::string &name() const;
	int id() const;

	virtual void init();
	virtual void run();

	void setRInterface(RInterface *r);
	void setDataSet(DataSet *dataSet);
	void setOptions(Options* options);

	enum Status { Empty, Initing, Inited, Running, Complete, Aborted };

	Status status();
	void setStatus(Status status);

protected:

	Status _status;

	Options* _options;
	DataSet *_dataSet;

	RInterface *_r;
	Json::Value _results;

	int callback(Json::Value results);

	static std::vector<std::string> list(
			std::string one,
			std::string two   = "",
			std::string three = "",
			std::string four  = "",
			std::string five  = "",
			std::string six   = "",
			std::string seven = "",
			std::string eight = "",
			std::string nine  = "",
			std::string ten   = "");

private:
	int _id;
	std::string _name;

	void optionsChangedHandler(Option *option);

};

#endif // ANALYSIS_H
