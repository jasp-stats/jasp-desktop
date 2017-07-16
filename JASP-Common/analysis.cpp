//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "analysis.h"

#include <boost/bind.hpp>
#include <boost/foreach.hpp>

#include "options/options.h"

using namespace boost::uuids;
using namespace boost;
using namespace std;

Analysis::Analysis(int id, string name, Options *options, Version version, bool autorun, bool usedata)
{
	_id = id;
	_name = name;
	_options = options;
	_autorun = autorun;
	_usedata = usedata;
	_version = version;

	_revision = 0;

	_options->changed.connect(boost::bind(&Analysis::optionsChangedHandler, this, _1));

	_status = Empty;
}

Analysis::~Analysis()
{
	delete _options;
}

void Analysis::abort()
{
	_status = Aborting;
	optionsChanged(this);
}

void Analysis::scheduleRun()
{
	if (_autorun)
		return;

	setStatus(Inited);
	optionsChanged(this);
}

void Analysis::setResults(Json::Value results)
{
	_results = results;
	resultsChanged(this);
}

void Analysis::setImageResults(Json::Value results)
{
	_imgResults = results;
	imageSaved(this);
}

void Analysis::setUserData(Json::Value userData, bool silient)
{
	_userData = userData;
	if ( ! silient)
		userDataLoaded(this);
}

const Json::Value &Analysis::results() const
{
	return _results;
}

const Json::Value &Analysis::userData() const
{
	return _userData;
}

void Analysis::refresh()
{
	_status = Empty;
	_revision++;
	toRefresh(this);
}

Analysis::Status Analysis::parseStatus(string name)
{
	if (name == "empty")
		return Analysis::Empty;
	else if (name == "waiting")
		return Analysis::Inited;
	else if (name == "running")
		return Analysis::Running;
	else if (name == "complete")
		return Analysis::Complete;
	else if (name == "aborted")
		return Analysis::Aborted;
	else if (name == "SaveImg")
		return Analysis::SaveImg;
	else if (name == "exception")
		return Analysis::Exception;
	else
		return Analysis::Error;
}

Json::Value Analysis::asJSON() const
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"] = _id;
	analysisAsJson["name"] = _name;
	analysisAsJson["version"] = _version.asString();
	analysisAsJson["results"] = _results;

	string status;

	switch (_status)
	{
	case Analysis::Empty:
		status = "empty";
		break;
	case Analysis::Inited:
		status = "waiting";
		break;
	case Analysis::Running:
		status = "running";
		break;
	case Analysis::Complete:
		status = "complete";
		break;
	case Analysis::Aborted:
		status = "aborted";
		break;
	case Analysis::SaveImg:
		status = "SaveImg";
	case Analysis::Exception:
		status = "exception";
		break;
	default:
		status = "error";
		break;
	}

	analysisAsJson["status"] = status;

	return analysisAsJson;
}

void Analysis::setVisible(bool visible)
{
	_visible = visible;
}

int Analysis::revision()
{
	return _revision;
}

bool Analysis::isVisible()
{
	return _visible;
}

bool Analysis::isRefreshBlocked()
{
	return _refreshBlocked;
}

void Analysis::setRefreshBlocked(bool block)
{
	_refreshBlocked = block;
}

Analysis::Status Analysis::status() const
{
	return _status;
}

void Analysis::setStatus(Analysis::Status status)
{
	_status = status;
}

const string &Analysis::name() const
{
	return _name;
}

int Analysis::id() const
{
	return _id;
}

bool Analysis::isAutorun() const
{
	return _autorun;
}

bool Analysis::useData() const
{
	return _usedata;
}

Options *Analysis::options() const
{
	return _options;
}

void Analysis::optionsChangedHandler(Option *option)
{
	if (_refreshBlocked)
		return;

	_status = Empty;
	_revision++;
	optionsChanged(this);
}

int Analysis::callback(Json::Value results)
{
	if (_status != Empty && _status != Aborted)
	{
		if (results != Json::nullValue)
		{
			_results = results;
			resultsChanged(this);
		}
		return 0;
	}
	else
	{
		return 1;
	}
}

void Analysis::setSaveImgOptions(Json::Value &options)
{
	_saveImgOptions = options;
}

Json::Value Analysis::getSaveImgOptions()
{
	return _saveImgOptions;
}

Json::Value Analysis::getImgResults()
{
	return _imgResults;
}
