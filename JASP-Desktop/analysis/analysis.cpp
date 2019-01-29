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


#include "options/options.h"
#include "tempfiles.h"
#include "appinfo.h"
#include "dirs.h"
#include "analyses.h"
#include "analysisform.h"

#include <QDebug>

using namespace boost::uuids;
using namespace boost;
using namespace std;

Analysis::Analysis(Analyses* analyses, size_t id, std::string module, std::string name, const Version &version, Json::Value *data)
	: QObject(analyses), _options(new Options()), _id(id), _module(module), _name(name), _title(name), _version(version), _analyses(analyses)
{
	if (data)
	{
		qDebug() << "Analysis data: " << QString::fromStdString(data->toStyledString());
		if (data->type() == Json::arrayValue)
			_options->init(*data);
		else
			_optionsFromJASPFile = *data;
	}

	bindOptionHandlers();
}

Analysis::Analysis(Analyses* analyses, size_t id, Modules::AnalysisEntry * analysisEntry)
	: QObject(analyses), _options(new Options()), _id(id), _name(analysisEntry->title()), _title(analysisEntry->title()), _version(AppInfo::version), _moduleData(analysisEntry), _analyses(analyses)
{
	bindOptionHandlers();
}

Analysis::~Analysis()
{
	delete _options;
}

void Analysis::bindOptionHandlers()
{
	_options->changed.connect(							boost::bind( &Analysis::optionsChangedHandler,						this, _1));
	_options->requestComputedColumnCreation.connect(	boost::bind( &Analysis::requestComputedColumnCreationHandler,		this, _1));
	_options->requestComputedColumnDestruction.connect(	boost::bind( &Analysis::requestComputedColumnDestructionHandler,	this, _1));
}

void Analysis::abort()
{
	_status = Aborting;
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
	if (name == "empty")				return Analysis::Empty;
	else if (name == "waiting")			return Analysis::Inited;
	else if (name == "running")			return Analysis::Running;
	else if (name == "complete")		return Analysis::Complete;
	else if (name == "aborted")			return Analysis::Aborted;
	else if (name == "SaveImg")			return Analysis::SaveImg;
	else if (name == "EditImg")			return Analysis::EditImg;
	else if (name == "exception")		return Analysis::Exception;
	else if (name == "initializing")	return Analysis::Initializing;
	else								return Analysis::Error;
}

void Analysis::initialized(AnalysisForm* form, bool isNewAnalysis)
{
	_analysisForm = form;
//		connect(form,			&AnalysisForm::formChanged, this,			&MainWindow::showForm);
	_status = isNewAnalysis ? Empty : Complete;
}

Json::Value Analysis::asJSON() const
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"]			= int(_id);
	analysisAsJson["name"]			= _name;
	analysisAsJson["title"]			= _title;
	analysisAsJson["rfile"]			= _rfile;
	analysisAsJson["module"]		= _module;
	analysisAsJson["progress"]		= _progress;
	analysisAsJson["version"]		= _version.asString();
	analysisAsJson["results"]		= _results;

	string status;

	switch (_status)
	{
	case Analysis::Empty:			status = "empty";		break;
	case Analysis::Inited:			status = "waiting";		break;
	case Analysis::Running:			status = "running";		break;
	case Analysis::Complete:		status = "complete";	break;
	case Analysis::Aborted:			status = "aborted";		break;
	case Analysis::SaveImg:			status = "SaveImg";		break;
	case Analysis::EditImg:			status = "EditImg";		break;
	case Analysis::Exception:		status = "exception";	break;
	case Analysis::Initializing:	status = "initializing"; break;
	default:						status = "error";		break;
	}

	analysisAsJson["status"]	= status;

	analysisAsJson["options"]	= options()->asJSON();
	analysisAsJson["userdata"]	= userData();

	if(_moduleData != nullptr)
		analysisAsJson["dynamicModule"] = _moduleData->asJsonForJaspFile();

	return analysisAsJson;
}

void Analysis::setStatus(Analysis::Status status)
{
	if ((status == Analysis::Running || status == Analysis::Initing) && _version != AppInfo::version)
	{
		TempFiles::deleteList(TempFiles::retrieveList(_id));
		_version = AppInfo::version;
	}
	_status = status;
}

DataSet *Analysis::getDataSet() const
{
	return _analyses->getDataSet();
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

std::string Analysis::qmlFormPath() const
{
	return "file:" + (_moduleData != nullptr	?
				_moduleData->qmlFilePath()	:
				Dirs::resourcesDir() + "/" + module() + "/qml/"  + name() + ".qml");
}

void Analysis::runScriptRequestDone(const QString& result, const QString& controlName)
{
	_analysisForm->runScriptRequestDone(result, controlName);
}

Json::Value Analysis::createAnalysisRequestJson(int ppi, std::string imageBackground)
{
	performType perform = desiredPerformTypeFromAnalysisStatus();

	switch(perform)
	{
	case performType::init:		setStatus(Analysis::Initing);	break;
	case performType::abort:	setStatus(Analysis::Aborted);	break;
	case performType::run:
	case performType::saveImg:
	case performType::editImg:	setStatus(Analysis::Running);	break;
	default:													break;
	}

	Json::Value json = Json::Value(Json::objectValue);

	json["typeRequest"]			= engineStateToString(engineState::analysis);
	json["id"]					= int(id());
	json["perform"]				= performTypeToString(perform);
	json["revision"]			= revision();
	json["rfile"]				= _moduleData == nullptr ? rfile() : "";
	json["jaspResults"]			= usesJaspResults();
	json["dynamicModuleCall"]	= _moduleData == nullptr ? "" : _moduleData->getFullRCall();


	if (!isAborted())
	{
		json["name"]			= name();
		json["title"]			= title();
		json["ppi"]				= ppi;
		json["imageBackground"] = imageBackground; //comes from engine representation!

		if (perform == performType::saveImg || perform == performType::editImg)
			json["image"] = getSaveImgOptions();
		else
		{
			json["options"]		= options()->asJSON();
		}
	}

	return json;
}

void Analysis::setName(std::string name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged();
}
