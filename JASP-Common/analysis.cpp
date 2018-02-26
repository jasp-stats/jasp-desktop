//
// Copyright (C) 2013-2018 University of Amsterdam
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
#include "tempfiles.h"
#include "appinfo.h"

using namespace boost::uuids;
using namespace boost;
using namespace std;

Analysis::Analysis(int id, string module, string name, string title, Json::Value &requiresInit, Json::Value &dataKey, Json::Value &stateKey, Json::Value &resultsMeta, Options *options, const Version &version, bool autorun, bool usedata, bool useJaspResults)
	: _options(options), _id(id), _module(module), _name(name), _title(title), _requiresInit(requiresInit), _dataKey(dataKey), _stateKey(stateKey), _resultsMeta(resultsMeta), _autorun(autorun), _usedata(usedata), _jaspResultsAnalysis(useJaspResults), _version(version)
{
	_options->changed.connect(boost::bind(&Analysis::optionsChangedHandler, this, _1));

	_status = Empty;
	_progress = -1;
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

void Analysis::setResults(Json::Value results, int progress)
{
	_results = results;
	_progress = progress;
	resultsChanged(this);
}

void Analysis::setImageResults(Json::Value results)
{
	_imgResults = results;
	imageSaved(this);
}

void Analysis::setImageEdited(Json::Value results)
{
    _imgResults = results;
    imageEdited(this);
}

void Analysis::setUserData(Json::Value userData)
{
	_userData = userData;
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
    else if (name == "EditImg")
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
	analysisAsJson["title"] = _title;
	analysisAsJson["requiresInit"] = _requiresInit;
	analysisAsJson["dataKey"] = _dataKey;
	analysisAsJson["stateKey"] = _stateKey;
	analysisAsJson["resultsMeta"] = _resultsMeta;
	analysisAsJson["module"] = _module;
	analysisAsJson["progress"] = _progress;
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
    case Analysis::EditImg:
        status = "EditImg";
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
	if ((status == Analysis::Running || status == Analysis::Initing) && _version != AppInfo::version)
	{
		tempfiles_deleteList(tempfiles_retrieveList(_id));
		_version = AppInfo::version;
	}
	_status = status;
}

const string &Analysis::name() const
{
	return _name;
}

const string &Analysis::title() const
{
	return _title;
}

const Json::Value &Analysis::requiresInit() const
{
	return _requiresInit;
}

const Json::Value &Analysis::dataKey() const
{
	return _dataKey;
}

const Json::Value &Analysis::stateKey() const
{
	return _stateKey;
}

const Json::Value &Analysis::resultsMeta() const
{
	return _resultsMeta;
}

const string &Analysis::module() const
{
	return _module;
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
