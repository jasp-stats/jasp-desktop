#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <boost/uuid/uuid.hpp>

#include <map>
#include <list>

#include "options/options.h"

#include "common.h"

class Analysis
{

public:
	Analysis(int id, std::string name, Options *options, bool isAutorun = true);
	virtual ~Analysis();

	Options *options() const;

	boost::signals2::signal<void (Analysis *source)> optionsChanged;
	boost::signals2::signal<void (Analysis *source)> resultsChanged;

	void setResults(Json::Value results);
	const Json::Value &results() const;
	Json::Value asJSON() const;

	const std::string &name() const;
	int id() const;
	bool isAutorun() const;

	virtual void abort();
	void scheduleRun();

	void setOptions(Options* options);

	enum Status { Empty, Initing, Inited, InitedAndWaiting, Running, Complete, Aborting, Aborted };

	Status status() const;
	void setStatus(Status status);

protected:

	Status _status;

	Options* _options;

	Json::Value _results;

	int callback(Json::Value results);

private:
	int _id;
	std::string _name;
	bool _autorun;

	void optionsChangedHandler(Option *option);

};

#endif // ANALYSIS_H
