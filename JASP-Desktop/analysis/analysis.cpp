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
#include "utilities/qutils.h"
#include "log.h"


Analysis::Analysis(Analyses* analyses, size_t id, std::string module, std::string name, std::string title, const Version &version, Json::Value *data) :
	QObject(analyses),
	_options(new Options()),
	_id(id),
	_module(module),
	_name(name),
	_titleDefault(title),
	_title(title),
	_version(version),
	_analyses(analyses)
{
	if (data)
		// We cannot set the options now because it sometimes needs more information from the QML file
		// (especially with OptionsTable that needs a template information).
		_optionsDotJASP = *data;

	setHelpFile("analyses/" + nameQ());
	bindOptionHandlers();
}

Analysis::Analysis(Analyses* analyses, size_t id, Modules::AnalysisEntry * analysisEntry, std::string title, Json::Value *data) :
	  QObject(analyses),
	  _options(new Options()),
	  _id(id),
	  _name(analysisEntry->title()),
	  _titleDefault(analysisEntry->title()),
	  _title(title == "" ? _titleDefault : title),
	  _version(AppInfo::version),
	  _moduleData(analysisEntry),
	  _dynamicModule(_moduleData->dynamicModule()),
	  _analyses(analyses)
{
	if (data)
		_optionsDotJASP = *data; //Same story as other constructor

	_codedReferenceToAnalysisEntry = analysisEntry->codedReference(); //We need to store this to be able to find the right analysisEntry after reloading the entries of a dynamic module (destroys analysisEntries)
	setHelpFile(dynamicModule()->helpFolderPath() + nameQ());
	bindOptionHandlers();
}

Analysis::Analysis(Analyses* analyses, size_t id, Analysis * duplicateMe)
	: QObject(			analyses					)
	, _status(			duplicateMe->_status		)
	, _options(			static_cast<Options*>(duplicateMe->_options->clone()))
	, _optionsDotJASP(	duplicateMe->_optionsDotJASP)
	, _results(			duplicateMe->_results		)
	, _imgResults(		duplicateMe->_imgResults	)
	, _userData(		duplicateMe->_userData		)
	, _imgOptions(		duplicateMe->_imgOptions	)
	, _progress(		duplicateMe->_progress		)
	, _id(				id							)
	, _module(			duplicateMe->_module		)
	, _name(			duplicateMe->_name			)
	, _titleDefault(	duplicateMe->_titleDefault	)
	, _title("Copy of "+duplicateMe->_title			)
	, _rfile(			duplicateMe->_rfile			)
	, _useJaspResults(	duplicateMe->_useJaspResults)
	, _isDuplicate(		true						)
	, _version(			duplicateMe->_version		)
	, _moduleData(		duplicateMe->_moduleData	)
	, _dynamicModule(	duplicateMe->_dynamicModule	)
	, _analyses(						analyses	)
	, _codedReferenceToAnalysisEntry(	duplicateMe->_codedReferenceToAnalysisEntry)
	, _helpFile(						duplicateMe->_helpFile)
{
	bindOptionHandlers();
}

Analysis::~Analysis()
{
	const auto & cols = columnsCreated();

	if(cols.size() > 0)
		for(const std::string & col : cols)
			emit requestComputedColumnDestruction(tq(col));

	delete _options;
}

void Analysis::clearOptions()
{
	_options->clear();
}

bool Analysis::checkAnalysisEntry()
{
	try
	{
		if(_codedReferenceToAnalysisEntry != "" && _dynamicModule != nullptr)
			_moduleData = _dynamicModule->retrieveCorrespondingAnalysisEntry(_codedReferenceToAnalysisEntry);
		return true;
	}
	catch (Modules::ModuleException & e)
	{
		return false;
	}
}

void Analysis::bindOptionHandlers()
{
	_options->changed.connect(							boost::bind( &Analysis::optionsChangedHandler,						this, _1));
	_options->requestComputedColumnCreation.connect(	boost::bind( &Analysis::requestComputedColumnCreationHandler,		this, _1));
	_options->requestColumnCreation.connect(			boost::bind( &Analysis::requestColumnCreationHandler,				this, _1, _2));
	_options->requestComputedColumnDestruction.connect(	boost::bind( &Analysis::requestComputedColumnDestructionHandler,	this, _1));
}

void Analysis::abort()
{
	setStatus(Aborting);
	emit optionsChanged(this);
}


void Analysis::setResults(const Json::Value & results, const Json::Value & progress)
{
	_results = results;
	_progress = progress;
	if (_analysisForm)
		_analysisForm->clearErrors();
	emit resultsChangedSignal(this);

	processResultsForDependenciesToBeShown();
}

void Analysis::imageSaved(const Json::Value & results)
{
	_imgResults = results;
	emit imageSavedSignal(this);
}

void Analysis::imageEdited(const Json::Value & results)
{
    _imgResults = results;
	emit imageEditedSignal(this);
}


void Analysis::reload()
{
	_analyses->reload(this, true);
}

void Analysis::exportResults()
{
    emit _analyses->analysesExportResults();
}

void Analysis::refresh()
{
	setStatus(Empty);
	TempFiles::deleteAll(_id);
	emit toRefreshSignal(this);

	if(_analysisForm)
		_analysisForm->refreshTableViewModels();
}

void Analysis::saveImage(const Json::Value &options)
{
	setStatus(Analysis::SaveImg);
	_imgOptions = options;
	emit saveImageSignal(this);
}

void Analysis::editImage(const Json::Value &options)
{
	setStatus(Analysis::EditImg);
	_imgOptions = options;
	emit editImageSignal(this);
}

void Analysis::rewriteImages()
{
	setStatus(Analysis::RewriteImgs);
	emit rewriteImagesSignal(this);
}

void Analysis::imagesRewritten()
{
	emit resultsChangedSignal(this);
}

Analysis::Status Analysis::parseStatus(std::string name)
{
	if		(name == "empty")			return Analysis::Empty;
	else if (name == "waiting")			return Analysis::Inited;
	else if (name == "running")			return Analysis::Running;
	else if (name == "complete")		return Analysis::Complete;
	else if (name == "initializing")	return Analysis::Initializing;
	else if (name == "RewriteImgs")		return Analysis::RewriteImgs;
	else if (name == "validationError")	return Analysis::ValidationError;
	else if (name == "aborted")			return Analysis::Aborted;
	else if (name == "SaveImg")			return Analysis::SaveImg;
	else if (name == "EditImg")			return Analysis::EditImg;
	else								return Analysis::FatalError;
}

void Analysis::initialized(AnalysisForm* form, bool isNewAnalysis)
{
						_analysisForm	= form;
	if(!_isDuplicate)	_status			= isNewAnalysis ? Empty : Complete;
	
	connect(_analyses, &Analyses::dataSetChanged,			_analysisForm, &AnalysisForm::dataSetChangedHandler);
	connect(_analyses, &Analyses::dataSetColumnsChanged,	_analysisForm, &AnalysisForm::dataSetChangedHandler); //Really should be renamed
}

std::string Analysis::statusToString(Status status)
{
	switch (status)
	{
	case Analysis::Empty:			return "empty";
	case Analysis::Inited:			return "inited";
	case Analysis::Initing:			return "initing";
	case Analysis::Running:			return "running";
	case Analysis::Complete:		return "complete";
	case Analysis::Aborted:			return "aborted";
	case Analysis::Aborting:		return "aborting";
	case Analysis::SaveImg:			return "SaveImg";
	case Analysis::EditImg:			return "EditImg";
	case Analysis::RewriteImgs:		return "RewriteImgs";
	case Analysis::ValidationError:	return "validationError";
	case Analysis::Initializing:	return "initializing";
	case Analysis::FatalError:		return "fatalError";
	default:						return "?????";
	}
}

Json::Value Analysis::asJSON() const
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"]			= int(_id);
	analysisAsJson["name"]			= _name;
	analysisAsJson["title"]			= _title;
	analysisAsJson["titleDef"]		= _titleDefault;
	analysisAsJson["rfile"]			= _rfile;
	analysisAsJson["module"]		= _module;
	analysisAsJson["progress"]		= _progress;
	analysisAsJson["version"]		= _version.asString();
	analysisAsJson["results"]		= _results;

	std::string status;

	switch (_status)
	{
	case Analysis::Empty:			status = "empty";			break;
	case Analysis::Inited:			status = "waiting";			break;
	case Analysis::Running:			status = "running";			break;
	case Analysis::Complete:		status = "complete";		break;
	case Analysis::Aborted:			status = "aborted";			break;
	case Analysis::SaveImg:			status = "SaveImg";			break;
	case Analysis::EditImg:			status = "EditImg";			break;
	case Analysis::RewriteImgs:		status = "RewriteImgs";		break;
	case Analysis::ValidationError:	status = "validationError";	break;
	case Analysis::Initializing:	status = "initializing";	break;
	default:						status = "fatalError";		break;
	}

	analysisAsJson["status"]		= status;
	analysisAsJson["options"]		= options()->asJSON();
	analysisAsJson["userdata"]		= userData();


	if(_moduleData != nullptr)
		analysisAsJson["dynamicModule"] = _moduleData->asJsonForJaspFile();

	return analysisAsJson;
}

void Analysis::loadExtraFromJSON(Json::Value & options)
{
	_titleDefault = options.get("titleDef", _titleDefault).asString();
	//The rest is already taken in from Analyses::createFromJaspFileEntry
}

void Analysis::setStatus(Analysis::Status status)
{
	if(_status == status)
		return;

	if ((status == Analysis::Running || status == Analysis::Initing) && _version != AppInfo::version)
	{
		TempFiles::deleteList(TempFiles::retrieveList(_id));
		_version = AppInfo::version;
	}

	_status = status;

	Log::log() << "Analysis " << title() << " (" << id() << ") now has status: " << statusToString(_status) << std::endl;
}

void Analysis::optionsChangedHandler(Option *option)
{
	if (_refreshBlocked)
		return;

	_status = Empty;
	optionsChanged(this);
}


int Analysis::callback(Json::Value results)
{
	if (_status != Empty && _status != Aborted)
	{
		if (results != Json::nullValue)
		{
			_results = results;
			resultsChangedSignal(this);
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
	case Analysis::RewriteImgs:	return(performType::rewriteImgs);
	case Analysis::Aborted:
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
	case performType::init:			setStatus(Analysis::Initing);	break;
	case performType::abort:		setStatus(Analysis::Aborted);	break;
	case performType::run:
	case performType::saveImg:
	case performType::editImg:
	case performType::rewriteImgs:	setStatus(Analysis::Running);	break;
	default:														break;
	}

	Json::Value json = Json::Value(Json::objectValue);

	json["typeRequest"]			= engineStateToString(engineState::analysis);
	json["id"]					= int(id());
	json["perform"]				= performTypeToString(perform);
	json["revision"]			= revision();
	json["rfile"]				= _moduleData == nullptr ? rfile() : "";
	json["jaspResults"]			= usesJaspResults();
	json["dynamicModuleCall"]	= _moduleData == nullptr ? "" : _moduleData->getFullRCall();
	json["developerMode"]		= _analyses->developerMode();

	if (!isAborted())
	{
		json["name"]			= name();
		json["title"]			= title();
		json["ppi"]				= ppi;
		json["imageBackground"] = imageBackground; //comes from engine representation!

		if (perform == performType::saveImg || perform == performType::editImg)
			json["image"] = imgOptions();
		else
		{
			json["options"]		= options()->size() == 0 ? optionsFromJASPFile() : options()->asJSON();
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

void Analysis::setHelpFile(QString helpFile)
{
	if (_helpFile == helpFile)
		return;

	_helpFile = helpFile;
	emit helpFileChanged(_helpFile);
}

void Analysis::setTitleQ(QString title)
{
	std::string strippedTitle	= title.simplified().toStdString();

	if(strippedTitle == "")
		strippedTitle = _titleDefault;

	if (_title == strippedTitle && strippedTitle == title.toStdString())
		return;

	_results["title"] = strippedTitle;
	_title = strippedTitle;
	
	emit titleChanged();
}

void Analysis::emitDuplicationSignals()
{
	emit resultsChangedSignal(this);
	emit titleChanged();
}

void Analysis::refreshAvailableVariablesModels()
{
	if(form() != nullptr)
		form()->refreshAvailableVariablesModels();
}

QString	Analysis::fullHelpPath(QString helpFileName)
{
	if(isDynamicModule())	return dynamicModule()->helpFolderPath() + helpFileName;
	else					return "analyses/" + helpFileName;
}

void Analysis::duplicateMe()
{
	_analyses->duplicateAnalysis(_id);
}


DataSetPackage * Analysis::getDataSetPackage() const
{
	return _analyses->getDataSetPackage();
}

void Analysis::showDependenciesOnQMLForObject(QString uniqueName)
{
	_showDepsName = uniqueName.toStdString();
	processResultsForDependenciesToBeShown();
}

bool Analysis::processResultsForDependenciesToBeShownMetaTraverser(const Json::Value & array)
{
	if(!array.isArray())
	{
		Log::log() << "metaTraverser in void Analysis::processResultsForDependenciesToBeShown() expects an array, but instead received: '" << array.toStyledString() << "'" << std::endl;
		return false;
	}

	for(const Json::Value & entry : array)
	{
		if(entry["name"].asString() == _showDepsName)
		{
			std::set<std::string> mustBe;
			for(const Json::Value & mustBeEntry : entry["mustBe"])
				mustBe.insert(mustBeEntry.asString());

			std::map<std::string, std::set<std::string>> mustContain;
			for(const std::string & mustContainName : entry["mustContain"].getMemberNames())
				for(const Json::Value & mustContainThis : entry["mustContain"][mustContainName])
					mustContain[mustContainName].insert(mustContainThis.asString());

			_analysisForm->setMustBe(mustBe);
			_analysisForm->setMustContain(mustContain);

			return true;
		}

		if(entry.isMember("meta") && processResultsForDependenciesToBeShownMetaTraverser(entry["meta"]))
			return true;
	}

	return false;
}

void Analysis::processResultsForDependenciesToBeShown()
{
	if(!_useJaspResults && _showDepsName != "")
		MessageForwarder::showWarning("Old-school analysis doesn't use dependencies", "You have tried to show the dependencies of an analysis that has not been rewritten to jaspResults, and seeing as how these older analyses do use dependencies there is nothing to show you...");

	if(!_results.isMember(".meta") || !_analysisForm)
		return;

	if(_showDepsName == "")
	{
		_analysisForm->setMustBe({});
		_analysisForm->setMustContain({});
		return;
	}

	processResultsForDependenciesToBeShownMetaTraverser(_results[".meta"]);
}

Json::Value Analysis::editOptionsOfPlot(const std::string & uniqueName)
{
	Json::Value editOptions = Json::nullValue;

	if(!_editOptionsOfPlot(_results, uniqueName, editOptions))
		MessageForwarder::showWarning("Could not find edit options of plot " + uniqueName + " so plot editing will not work...");

	return editOptions;
}

bool Analysis::_editOptionsOfPlot(const Json::Value & results, const std::string & uniqueName, Json::Value & editOptions)
{
	if(results.isArray())
		for(const Json::Value & entry : results)
			if(_editOptionsOfPlot(entry, uniqueName, editOptions))
				return true;

	if(results.isObject())
	{
		if(results.isMember("name") && results["name"].asString() == uniqueName && results.isMember("editOptions"))
		{
			editOptions = results["editOptions"];

			Log::log() << "Found editOptions of " << uniqueName << " and they are:\n" << editOptions.toStyledString() << std::endl;

			return true;
		}

		for(const std::string & member : results.getMemberNames())
			if(_editOptionsOfPlot(results[member], uniqueName, editOptions))
				return true;
	}

	return false;
}

void Analysis::setEditOptionsOfPlot(const std::string & uniqueName, const Json::Value & editOptions)
{
	if(!_setEditOptionsOfPlot(_results, uniqueName, editOptions))
		MessageForwarder::showWarning("Could not find set edit options of plot " + uniqueName + " so plot editing will not remember anything (if it evens works)...");
}

bool Analysis::_setEditOptionsOfPlot(Json::Value & results, const std::string & uniqueName, const Json::Value & editOptions)
{
	if(results.isArray())
		for(Json::Value & entry : results)
			if(_setEditOptionsOfPlot(entry, uniqueName, editOptions))
				return true;

	if(results.isObject())
	{
		if(results.isMember("name") && results["name"].asString() == uniqueName && results.isMember("editOptions"))
		{
			Log::log() << "Replacing editOptions of " << uniqueName << ", old:\n" << results["editOptions"].toStyledString() << "\nnew:\n" << editOptions.toStyledString() << std::endl;
			results["editOptions"] = editOptions;
			return true;
		}

		for(const std::string & member : results.getMemberNames())
			if(_setEditOptionsOfPlot(results[member], uniqueName, editOptions))
				return true;
	}

	return false;
}
