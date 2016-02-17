//
// Copyright (C) 2013-2016 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

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
	boost::signals2::signal<void (Analysis *source)> userDataLoaded;

	void setResults(Json::Value results);
	void setUserData(Json::Value userData, bool silient = false);
	const Json::Value &results() const;
	const Json::Value &userData() const;
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
	Json::Value _userData;

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
