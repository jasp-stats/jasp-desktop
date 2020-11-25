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


Analysis::Analysis(size_t id, std::string module, std::string name, std::string qml, std::string title, const Version &version, Json::Value *data) :
	QObject(Analyses::analyses()),
	_options(new Options()),
	_id(id),
	_module(module),
	_name(name),
	_qml(qml),
	_titleDefault(title),
	_title(title),
	_version(version)
{
	if (data)
		// We cannot set the options now because it sometimes needs more information from the QML file
		// (especially with OptionsTable that needs a template information).
		_optionsDotJASP = *data;

	setHelpFile("analyses/" + nameQ());
	bindOptionHandlers();
}

Analysis::Analysis(size_t id, Modules::AnalysisEntry * analysisEntry, std::string title, std::string moduleVersion, Json::Value *data) :
	  QObject(Analyses::analyses()),
	  _options(new Options()),
	  _id(id),
	  _name(analysisEntry->title()),
	  _qml(analysisEntry->qml().empty() ? _name : analysisEntry->qml()),
	  _titleDefault(analysisEntry->title()),
	  _title(title == "" ? _titleDefault : title),
	  _moduleVersion(moduleVersion),
	  _version(AppInfo::version),
	  _moduleData(analysisEntry),
	  _dynamicModule(_moduleData->dynamicModule())
{
	if(_moduleVersion == "" && _dynamicModule)
		_moduleVersion = _dynamicModule->version();

	if (data)
		_optionsDotJASP = *data; //Same story as other constructor

	_codedReferenceToAnalysisEntry = analysisEntry->codedReference(); //We need to store this to be able to find the right analysisEntry after reloading the entries of a dynamic module (destroys analysisEntries)
	setHelpFile(dynamicModule()->helpFolderPath() + tq(analysisEntry->function()));
	bindOptionHandlers();
}

Analysis::Analysis(size_t id, Analysis * duplicateMe)
	: QObject(			Analyses::analyses()		)
	, _status(			duplicateMe->_status		)
	, _options(			static_cast<Options*>(duplicateMe->_options->clone()))
	, _optionsDotJASP(	duplicateMe->_optionsDotJASP)
	, _results(			duplicateMe->_results		)
	, _meta(			_results.get(".meta", Json::arrayValue))
	, _imgResults(		duplicateMe->_imgResults	)
	, _userData(		duplicateMe->_userData		)
	, _imgOptions(		duplicateMe->_imgOptions	)
	, _progress(		duplicateMe->_progress		)
	, _id(				id							)
	, _module(			duplicateMe->_module		)
	, _name(			duplicateMe->_name			)
	, _qml(				duplicateMe->_qml			)
	, _titleDefault(	duplicateMe->_titleDefault	)
	, _title("Copy of "+duplicateMe->_title			)
	, _rfile(			duplicateMe->_rfile			)
	, _isDuplicate(		true						)
	, _version(			duplicateMe->_version		)
	, _moduleData(		duplicateMe->_moduleData	)
	, _dynamicModule(	duplicateMe->_dynamicModule	)
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

QString Analysis::helpMD() const
{
	return _analysisForm ? _analysisForm->helpMD() : "";
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

void Analysis::remove()
{
	abort();
	if (form())
		form()->cleanUpForm();
}


void Analysis::setResults(const Json::Value & results, Status status, const Json::Value & progress)
{
	_results	= results;
	_progress	= progress;
	_meta		= _results.get(".meta", Json::arrayValue);

	setStatus(status);

	emit resultsChangedSignal(this);

	processResultsForDependenciesToBeShown();

	_wasUpgraded = false;
}

void Analysis::reload()
{
	Analyses::analyses()->reload(this, true);
}

void Analysis::rebind()
{
	if (_analysisForm)
		_analysisForm->bindTo();
}

void Analysis::exportResults()
{
	emit Analyses::analyses()->analysesExportResults();
}

void Analysis::run()
{
	setStatus(Empty);
}

void Analysis::refresh()
{
	TempFiles::deleteAll(_id);
	run();

	if(_analysisForm)
		_analysisForm->refreshTableViewModels();
}

void Analysis::saveImage(const Json::Value &options)
{
	setStatus(Analysis::SaveImg);
	_imgOptions = options;
}

void Analysis::imageSaved(const Json::Value & results)
{
	_imgResults = results;

	setStatus(Analysis::Complete);

	emit imageSavedSignal(this);
}

void Analysis::editImage(const Json::Value &options)
{
	setStatus(Analysis::EditImg);
	_imgOptions = options;
}

void Analysis::imageEdited(const Json::Value & results)
{
	_imgResults = results;

	if(		 _imgResults.get(	"resized",	false).asBool()		&&
			!_imgResults.get(	"error",	true).asBool()		&&
			_imgOptions.get(	"name",		"").asString() != "" )
		updatePlotSize(_imgOptions["name"].asString(), _imgResults.get("width", -1).asInt(), _imgResults.get("height", -1).asInt(), _results);

	setStatus(Analysis::Complete);

	emit imageEditedSignal(this);
	emit imageChanged();

	//Maybe this is the wrong request, because it took a while and the user kept changing stuff in the ploteditor
	if(_imgOptions.isMember("request") && _imgResults.isMember("request") && _imgOptions["request"].asInt() != _imgResults["request"].asInt())
		editImage(_imgOptions);
}

bool Analysis::updatePlotSize(const std::string & plotName, int width, int height, Json::Value & root)
{
	if(root.isNull()) return false;

	if(root.isArray())
		for(Json::Value & entry: root)
			if(updatePlotSize(plotName, width, height, entry))
				return true;

	if(root.isObject())
	{
		if(root.isMember(plotName))
		{
			root[plotName]["width"]  = width;
			root[plotName]["height"] = height;
			return true;
		}
		else
			for(const std::string & memName : root.getMemberNames())
				if(updatePlotSize(plotName, width, height, root[memName]))
					return true;
	}

	return false;
}


void Analysis::rewriteImages()
{
	setStatus(Analysis::RewriteImgs);
}

void Analysis::imagesRewritten()
{
	setStatus(Analysis::Complete);
	emit resultsChangedSignal(this);
	emit imageChanged();

}

Analysis::Status Analysis::parseStatus(std::string name)
{
	if		(name == "empty")			return Analysis::Empty;
	else if (name == "waiting")			return Analysis::Running; //For backwards compatibility
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

	if(!_isDuplicate && isNewAnalysis)
		_status = Empty;
	
	connect(Analyses::analyses(),	&Analyses::dataSetChanged,			_analysisForm,	&AnalysisForm::dataSetChangedHandler		);
	connect(Analyses::analyses(),	&Analyses::dataSetColumnsChanged,	_analysisForm,	&AnalysisForm::dataSetColumnsChangedHandler	);
	connect(_analysisForm,			&AnalysisForm::helpMDChanged,		this,			&Analysis::helpMDChanged					);
}


Analysis::Status Analysis::analysisResultsStatusToAnalysisStatus(analysisResultStatus result)
{
	switch(result)
	{
	case analysisResultStatus::changed:			return Analysis::KeepStatus; //changed is returned by the engine when it was killed by us, the desktop, so we return KeepStatus which tells the analysis that it shouldn't change status. A bit unwieldy but requires least changes.
	case analysisResultStatus::validationError:	return Analysis::ValidationError;
	case analysisResultStatus::fatalError:		return Analysis::FatalError;
	case analysisResultStatus::imageSaved:
	case analysisResultStatus::imageEdited:
	case analysisResultStatus::imagesRewritten:
	case analysisResultStatus::complete:		return Analysis::Complete;
	case analysisResultStatus::running:			return Analysis::Running;
	default:									throw std::logic_error("When you define new analysisResultStatuses like '" + analysisResultStatusToString(result)  +  "' you should add them to EngineRepresentation::analysisResultStatusToAnalysStatus!");
	}
}

std::string Analysis::statusToString(Status status)
{
	switch (status)
	{
	case Analysis::Empty:			return "empty";
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

	Log::log() << "Analysis::asJSON():\n" << analysisAsJson.toStyledString() << std::endl;

	return analysisAsJson;
}

void Analysis::loadExtraFromJSON(Json::Value & analysisData)
{
	_titleDefault	= analysisData.get("titleDef", _titleDefault).asString();
	_oldVersion		= analysisData.get("preUpgradeVersion", _results.get("version", AppInfo::version.asString())).asString();

	Log::log() << "Now loading userdata and results for analysis " << _name << " from file." << std::endl;
	setUserData(analysisData["userdata"]);
	setResults(analysisData["results"], _status);

	//The rest is already taken in from Analyses::createFromJaspFileEntry
}

void Analysis::setStatus(Analysis::Status status)
{
	if(status == Analysis::KeepStatus)
	{
		Log::log() << "Analysis " << _id << " '" << _title << "' got setStatus(KeepStatus) so it ignores it." << std::endl;
		return;
	}

	if(_status == status)
		return;

	//Make sure old notes etc aren't lost on table/plot-renames, see: https://github.com/jasp-stats/jasp-test-release/issues/469
	if(_status == Analysis::Complete)									storeUserDataEtc();
	if( status == Analysis::Complete && _status == Analysis::Running)	fitOldUserDataEtc();

	if (status == Analysis::Running && needsRefresh())
	{
		bool neededRefresh = needsRefresh();

		TempFiles::deleteList(TempFiles::retrieveList(_id));
		setVersion(AppInfo::version, true);

		if(_dynamicModule)
			_moduleVersion = _dynamicModule->version();

		if(neededRefresh != needsRefresh())
			emit needsRefreshChanged();
	}

	_status = status;

	Log::log() << "Analysis " << title() << " (" << id() << ") now has status: " << statusToString(_status) << std::endl;

	emit statusChanged(this);
}

void Analysis::optionsChangedHandler(Option *option)
{
	incrementRevision(); // To make sure we always process all changed options we increment the revision whenever anything changes

	Log::log() << "Option changed for analysis '" << name() << "' and id " << id() << ", revision incremented to: " << _revision << " and options are now: " << option->asJSON().toStyledString() << std::endl;

	if (_refreshBlocked)
		return;

	if (form() && (form()->hasError() || !form()->runWhenThisOptionIsChanged(option)))
		return;

	run();
}

ComputedColumn *Analysis::requestComputedColumnCreationHandler(std::string columnName)
{
	ComputedColumn *result = requestComputedColumnCreation(tq(columnName), this);

	if (result && form())
		form()->addOwnComputedColumn(tq(columnName));

	return result;
}

void Analysis::requestComputedColumnDestructionHandler(std::string columnName)
{
	requestComputedColumnDestruction(tq(columnName));

	if (form())
		form()->removeOwnComputedColumn(tq(columnName));
}

performType Analysis::desiredPerformTypeFromAnalysisStatus() const
{
	switch(status())
	{
	case Analysis::Empty:		return(performType::run);
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
				Dirs::resourcesDir() + "/" + module() + "/qml/"  + qml());
}

void Analysis::replaceVariableName(const std::string & oldName, const std::string & newName)
{
	if (_options)
		_options->replaceVariableName(oldName, newName);

	if (_analysisForm)
		_analysisForm->replaceVariableNameInListModels(oldName, newName);
}

void Analysis::runScriptRequestDone(const QString& result, const QString& controlName)
{
	if (_analysisForm)
		_analysisForm->runScriptRequestDone(result, controlName);
}

Json::Value Analysis::createAnalysisRequestJson()
{
	performType perform = desiredPerformTypeFromAnalysisStatus();

	switch(perform)
	{
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
	json["dynamicModuleCall"]	= _moduleData == nullptr ? "" : _moduleData->getFullRCall();

	if (!isAborted())
	{
		json["name"]			= name();
		json["title"]			= title();

		bool imgP = perform == performType::saveImg || perform == performType::editImg;
		if (imgP)	json["image"]		= imgOptions();
		else		json["options"]		= options()->size() == 0 ? optionsFromJASPFile() : options()->asJSONWithMeta();
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
	emit helpFileChanged();
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
	Analyses::analyses()->duplicateAnalysis(_id);
}

void Analysis::showDependenciesOnQMLForObject(QString uniqueName)
{
	_showDepsName = uniqueName.toStdString();
	processResultsForDependenciesToBeShown();
}

void Analysis::setOptionsBound(bool optionsBound)
{
	if (_optionsBound == optionsBound)
		return;

	_optionsBound = optionsBound;
	emit optionsBoundChanged(_optionsBound);
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
		MessageForwarder::showWarning(tr("Could not find edit options of plot %1 so plot editing will not work...").arg(tq(uniqueName)));

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
		MessageForwarder::showWarning(tr("Could not find set edit options of plot %1 so plot editing will not remember anything (if it evens works)...").arg(tq(uniqueName)));
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

void Analysis::setErrorInResults(const std::string & msg)
{
	Json::Value errorResults		= Json::objectValue;
	errorResults["error"]			= 1;
	errorResults["errorMessage"]	= msg;
	errorResults["title"]			= title();

	setResults(errorResults, Status::FatalError);
}

std::string Analysis::upgradeMsgsForOption(const std::string & name) const
{
	if(_msgs.count(name) == 0)
		return "";

	std::stringstream out;
	const std::vector<std::string> & msg = _msgs.at(name);

	for(size_t i=0; i<msg.size(); i++)
		out << (i > 0 ? "\n" : "") << msg[i];

	return out.str();
}

void Analysis::storeUserDataEtc()
{
	if(!needsRefresh())
			return;

	_oldUserData = _userData,
	_oldMetaData = _results.get(".meta", Json::arrayValue);

	_tryToFixNotes = !_userData.isNull();
}

void Analysis::fitOldUserDataEtc()
{
	if(!_tryToFixNotes)
		return;

	_tryToFixNotes = false;

	try {
		const Json::Value & currMetaData = _results.get(".meta", Json::arrayValue);

		std::map<std::string, std::string> oldToNew;

		//Only do special fix for older ANOVA's
		if(module() == "ANOVA" && Modules::Version(_oldVersion) < Modules::Version("0.12"))
		{
			//Gotta do some manual repairing for https://github.com/jasp-stats/jasp-test-release/issues/649
			//All of these replacements are based on the unittests.

			if(name() == "AnovaRepeatedMeasures")
			{
				oldToNew =
				{
					{ "assumptionsObj",			"rmAnovaContainer_assumptionsContainer"						},
					{ "sphericity",				"rmAnovaContainer_assumptionsContainer_sphericityTable"		},
					{ "levene",					"rmAnovaContainer_assumptionsContainer_rmAnovaLevenesTable"	},
					{ "posthoc",				"rmAnovaContainer_postHocStandardContainer"					},
					{ "withinSubjectsEffects",	"rmAnovaContainer_withinAnovaTable"							},
					{ "betweenSubjectsEffects",	"rmAnovaContainer_betweenTable"								},
					{ "simpleEffects",			"rmAnovaContainer_simpleEffectsContainer_simpleEffectsTable"},
					{ "descriptivesTable",		"rmAnovaContainer_descriptivesContainer_tableDescriptives"	},
					{ "descriptivesObj",		"rmAnovaContainer_descriptivesContainer"					},
					{ "marginalMeans",			"rmAnovaContainer_marginalMeansContainer"					},
					{ "contrasts",				"rmAnovaContainer_contrastContainer"						},
					{ "friedman",				"rmAnovaContainer_nonparametricContainer_friedmanTable"		},
					{ "conover",				"rmAnovaContainer_nonparametricContainer_conoverContainer"	}
				};
			}
			else if(name() == "Ancova" || name() == "Anova")
			{
				oldToNew =
				{
					{ "anova",					"anovaContainer_anovaTable"									},
					{ "assumptionsObj",			"anovaContainer_assumptionsContainer"						},
					{ "sphericity",				"anovaContainer_assumptionsContainer_sphericityTable"		},
					{ "levene",					"anovaContainer_assumptionsContainer_rmAnovaLevenesTable"	},
					{ "posthoc",				"anovaContainer_postHocStandardContainer"					},
					{ "withinSubjectsEffects",	"anovaContainer_withinAnovaTable"							},
					{ "betweenSubjectsEffects",	"anovaContainer_betweenTable"								},
					{ "simpleEffects",			"anovaContainer_simpleEffectsContainer_simpleEffectsTable"	},
					{ "descriptivesTable",		"anovaContainer_descriptivesContainer_tableDescriptives"	},
					{ "descriptivesObj",		"anovaContainer_descriptivesContainer"						},
					{ "marginalMeans",			"anovaContainer_marginalMeansContainer"						},
					{ "contrasts",				"anovaContainer_contrastContainer"							},
					{ "friedman",				"anovaContainer_nonparametricContainer_friedmanTable"		},
					{ "conover",				"anovaContainer_nonparametricContainer_conoverContainer"	}
				};
			}

			/*else if(name() == "")
			{
				oldToNew =
				{

				};
			}*/
		}

		if(oldToNew.size() == 0) //little algorithm to fix misplaced notes for https://github.com/jasp-stats/jasp-test-release/issues/469
		{
			std::vector<std::string> oldTables, newTables, oldPlots, newPlots, oldCollections, newCollections;

			std::function<void					(const std::set<std::string> & elementTypes, std::vector<std::string> & elements, const Json::Value & meta)  > extractAndFlattenElements
				= [&extractAndFlattenElements]	(const std::set<std::string> & elementTypes, std::vector<std::string> & elements, const Json::Value & meta) -> void
			{
				//Log::log() << "Meta to flatten: " << meta.toStyledString() << std::endl;

				for(const Json::Value & entry : meta)
					if(entry.isArray()) // Sometimes some old files follow a non-monotonous pattern of meta-nesting :s
						extractAndFlattenElements(elementTypes, elements, entry);
					else if(entry.isObject())
					{
						if(elementTypes.count(entry.get("type", "").asString()) > 0)		elements.push_back(entry["name"].asString());
						if(entry.isMember("meta"))											extractAndFlattenElements(elementTypes, elements, entry["meta"]);
					}
			};

			extractAndFlattenElements({"image"},				oldPlots,		_oldMetaData);
			extractAndFlattenElements({"image"},				newPlots,		currMetaData);
			extractAndFlattenElements({"table"},				oldTables,		_oldMetaData);
			extractAndFlattenElements({"table"},				newTables,		currMetaData);
			extractAndFlattenElements({"collection", "object"},	oldCollections, _oldMetaData);
			extractAndFlattenElements({"collection", "object"},	newCollections, currMetaData);


			for(size_t i=0; i<oldPlots.size()		&& i<newPlots.size();		i++)	oldToNew[ oldPlots[i]		] = newPlots[i];
			for(size_t i=0; i<oldTables.size()		&& i<newTables.size();		i++)	oldToNew[ oldTables[i]		] = newTables[i];
			for(size_t i=0; i<oldCollections.size() && i<newCollections.size();	i++)	oldToNew[ oldCollections[i]	] = newCollections[i];
		}

		std::map<std::string, Json::Value>	nameToNote, orphanedNotes;


		std::function<void	(const Json::Value & userData, const std::function<std::string(const std::string &)> & nameConvertor)  > extractVolatileNotes
			= [&]			(const Json::Value & userData, const std::function<std::string(const std::string &)> & nameConvertor) -> void
		{
			if(userData.isNull())
				return;

			if(userData.isObject() && userData.isMember("children"))
				for(const std::string & childName : userData["children"].getMemberNames())
				{
					const Json::Value & child = userData["children"][childName];

					if(child.isMember("note"))
					{
						const std::string convertedName = nameConvertor(childName);

						if(nameToNote.count(convertedName) == 0)	nameToNote[convertedName] = child["note"];
						else if(convertedName != childName)			orphanedNotes[childName]  = child["note"]; //Only if converted name is different. Because otherwise it totally makes sense that the note is already filled, javascript will have done that.
					}

					extractVolatileNotes(child, nameConvertor);
				}
		};

		extractVolatileNotes(_userData,		[](const std::string & in) { return in; });
		extractVolatileNotes(_oldUserData, [&](const std::string & in) { return oldToNew.count(in) > 0 ? oldToNew[in] : "???"; });

		//Now we just need to convert these notes back to proper userData...
		std::function<Json::Value						(const Json::Value & meta)  > createNewUserDataChildren =
			[&createNewUserDataChildren, &nameToNote]	(const Json::Value & meta) -> Json::Value
		{
			Json::Value out(Json::objectValue);

			for(const Json::Value & entry : meta)
			{
				const std::string	name		= entry["name"].asString();
				Json::Value			children	= !entry.isMember("meta") ? Json::nullValue : createNewUserDataChildren(entry["meta"]);

				if(!children.isNull() || nameToNote.count(name) > 0)	out[name]				= Json::objectValue;
				if(!children.isNull())									out[name]["children"]	= children;
				if(nameToNote.count(name) > 0)							out[name]["note"]		= nameToNote[name];
			}

			if(out.size() == 0)	return Json::nullValue;
			else				return out;
		};

		Json::Value newUserdataChildren = createNewUserDataChildren(currMetaData);

		if(!newUserdataChildren.isNull())	_userData["children"]	= newUserdataChildren;
		else								_userData.removeMember("children");


		Json::Value orphans = Json::objectValue;
		for(const auto & nameNote : orphanedNotes)
		{
			Json::Value orphan					= Json::objectValue;
						orphan["note"]			= nameNote.second;
						orphans[nameNote.first]	= orphan;
		}

		if(orphans.size() > 0)
			_userData["orphanedNotes"] = orphans;

		emit userDataChangedSignal(this);
		Log::log() << "New userData after attempt to fix is: " << _userData.toStyledString() << std::endl;

	} catch (...) {
		//It's ok if this fails, we are just trying to fix the notes, no guarantees.
		Log::log() << "Trying to fix the notes for analysis (" << name() << " # " << id() << ") had an error, maybe something changed in the meta or the userdata? Or this is a very old jasp-file?" << std::endl;
	}
}

void Analysis::setUpgradeMsgs(const Modules::UpgradeMsgs &msgs)
{
	bool wasRefreshNeeded = needsRefresh();

	_msgs = msgs;
	_wasUpgraded = true;

	if(!wasRefreshNeeded)
		emit needsRefreshChanged();
}

void Analysis::setVersion(Version version, bool resetWasUpgraded)
{
	bool oldNeedsRefresh = needsRefresh();

	if(resetWasUpgraded)
		_wasUpgraded = false;

	_version = version;

	if(needsRefresh() != oldNeedsRefresh)
		emit needsRefreshChanged();
}

bool Analysis::needsRefresh() const
{
	bool differentVersion = _dynamicModule ? _moduleVersion != _dynamicModule->version() : version() != AppInfo::version;
	return _wasUpgraded || differentVersion;
}


void Analysis::setHasVolatileNotes(bool hasVolatileNotes)
{
	if (_hasVolatileNotes == hasVolatileNotes)
		return;

	_hasVolatileNotes = hasVolatileNotes;
	emit hasVolatileNotesChanged(_hasVolatileNotes);
}


void Analysis::setUserData(Json::Value userData)
{
	_userData = userData;

	std::function<bool(const Json::Value & userData)> checkForVolatileNotes = [&checkForVolatileNotes](const Json::Value & userData) -> bool
	{
		if(!userData.isMember("children"))
			return false;

		for(const Json::Value & child : userData["children"])
			if(child.isMember("note") || checkForVolatileNotes(child))
				return true;

		return false;
	};

	setHasVolatileNotes(checkForVolatileNotes(_userData));
}

void Analysis::setDynamicModule(Modules::DynamicModule * module)
{
	Log::log() << "Replacing module connected to analysis " << title() << " (" << id() << ") for module " << module->name() << std::endl;
	_dynamicModule = module;

	checkAnalysisEntry();
}
