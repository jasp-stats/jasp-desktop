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

Analysis::Analysis(int id, string module, string name, string title, Json::Value &requiresInit, Json::Value &dataKey, Json::Value &stateKey, Json::Value &resultsMeta, Json::Value optionsJson, const Version &version, Json::Value *data, bool autorun, bool usedata, bool useJaspResults)
	: _id(id), _module(module), _name(name), _title(title), _requiresInit(requiresInit), _dataKey(dataKey), _stateKey(stateKey), _resultsMeta(resultsMeta), _autorun(autorun), _usedata(usedata), _jaspResultsAnalysis(useJaspResults), _version(version)
{
	_options = new Options();

	if (optionsJson != Json::nullValue)	_options->init(optionsJson);
	else								perror("malformed analysis definition");
	if (data != NULL)					_options->set(*data);

	_options->changed.connect(							boost::bind( &Analysis::optionsChangedHandler,						this, _1));
	_options->requestComputedColumnCreation.connect(	boost::bind( &Analysis::requestComputedColumnCreationHandler,		this, _1));
	_options->requestComputedColumnDestruction.connect(	boost::bind( &Analysis::requestComputedColumnDestructionHandler,	this, _1));

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


void Analysis::refresh()
{
	_status = Empty;
	_revision++;
	toRefresh(this);
}

Analysis::Status Analysis::parseStatus(string name)
{
	if (name == "empty")			return Analysis::Empty;
	else if (name == "waiting")		return Analysis::Inited;
	else if (name == "running")		return Analysis::Running;
	else if (name == "complete")	return Analysis::Complete;
	else if (name == "aborted")		return Analysis::Aborted;
	else if (name == "SaveImg")		return Analysis::SaveImg;
	else if (name == "EditImg")		return Analysis::SaveImg;
	else if (name == "exception")	return Analysis::Exception;
	else							return Analysis::Error;
}

Json::Value Analysis::asJSON() const
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"]			= _id;
	analysisAsJson["name"]			= _name;
	analysisAsJson["title"]			= _title;
	analysisAsJson["requiresInit"]	= _requiresInit;
	analysisAsJson["dataKey"]		= _dataKey;
	analysisAsJson["stateKey"]		= _stateKey;
	analysisAsJson["resultsMeta"]	= _resultsMeta;
	analysisAsJson["module"]		= _module;
	analysisAsJson["progress"]		= _progress;
	analysisAsJson["version"]		= _version.asString();
	analysisAsJson["results"]		= _results;

	string status;

	switch (_status)
	{
	case Analysis::Empty:		status = "empty";		break;
	case Analysis::Inited:		status = "waiting";		break;
	case Analysis::Running:		status = "running";		break;
	case Analysis::Complete:	status = "complete";	break;
	case Analysis::Aborted:		status = "aborted";		break;
	case Analysis::SaveImg:		status = "SaveImg";		break;
	case Analysis::EditImg:		status = "EditImg";		break;
	case Analysis::Exception:	status = "exception";	break;
	default:					status = "error";		break;
	}

	analysisAsJson["status"] = status;

	return analysisAsJson;
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


performType Analysis::desiredPerformTypeFromAnalysisStatus() const
{
	switch(status())
	{
	case Analysis::Empty:		return(usesJaspResults() ? performType::run : performType::init);
	case Analysis::SaveImg:		return(performType::saveImg);
	case Analysis::EditImg:		return(performType::editImg);
	case Analysis::Aborting:	return(performType::abort);
	default:					return(performType::run);
	}
}

