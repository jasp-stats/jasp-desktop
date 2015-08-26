#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <boost/uuid/uuid.hpp>

#include <map>
#include <list>

#include "options/options.h"

#include "common.h"
#include "version.h"


class Analysis
{
public:

	enum Status { Empty, Initing, Inited, InitedAndWaiting, Running, Complete, Aborting, Aborted, Error };

	Analysis(int id, std::string name, Options *options, Version version, bool isAutorun = true);
	virtual ~Analysis();

	Options *options() const;

	boost::signals2::signal<void (Analysis *source)> optionsChanged;
	boost::signals2::signal<void (Analysis *source)> resultsChanged;
	boost::signals2::signal<void (Analysis *source)> notesLoaded;

	void setResults(Json::Value results);
	void setNotes(Json::Value notes, bool silient = false);
	const Json::Value &results() const;
	const Json::Value &notes() const;
	Json::Value asJSON() const;

	const std::string &name() const;
	int id() const;
	bool isAutorun() const;

	virtual void abort();
	void scheduleRun();

	Status status() const;
	void setStatus(Status status);

	bool isVisible();
	void setVisible(bool visible);

	int revision();

	static Status parseStatus(std::string name);

protected:

	Status _status;
	bool _visible = true;

	Options* _options;

	Json::Value _results;
	Json::Value _notes;

	int callback(Json::Value results);

private:

	std::string _name;
	int _id;
	bool _autorun;
	Version _version;

	int _revision;

	void optionsChangedHandler(Option *option);

};

#endif // ANALYSIS_H
