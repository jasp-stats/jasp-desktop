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
#include "tempfiles.h"
#include "appinfo.h"
#include "dirs.h"
#include "analyses.h"
#include "analysisform.h"
#include "utilities/qutils.h"
#include "log.h"
#include "utils.h"
#include "utilities/settings.h"
#include "utilities/qutils.h"
#include "gui/preferencesmodel.h"


Analysis::Analysis(size_t id, Modules::AnalysisEntry * analysisEntry, std::string title, std::string moduleVersion, Json::Value *data) :
	  AnalysisBase(Analyses::analyses()),
	  _id(				id),
	  _name(			analysisEntry->function()),
	  _qml(				analysisEntry->qml().empty() ? _name : analysisEntry->qml()),
	  _titleDefault(	analysisEntry->title()),
	  _title(			title == "" ? _titleDefault : title),
	  _moduleVersion(	moduleVersion),
	  _version(			AppInfo::version),
	  _moduleData(		analysisEntry),
	  _dynamicModule(	_moduleData->dynamicModule())
{
	if(_moduleVersion == "" && _dynamicModule)
		_moduleVersion = _dynamicModule->version();

	if (data)
		_optionsDotJASP = *data; //Same story as other constructor

	_codedAnalysisEntry = analysisEntry->codedReference(); //We need to store this to be able to find the right analysisEntry after reloading the entries of a dynamic module (destroys analysisEntries). Or replacing the entry if a different version of the module gets loaded of course.
	_helpFile = dynamicModule()->helpFolderPath() + tq(analysisEntry->function());

	initAnalysis();
}

Analysis::Analysis(size_t id, Analysis * duplicateMe)
	: AnalysisBase(			Analyses::analyses()					)
	, _status(				duplicateMe->_status					)
	, _boundValues(			duplicateMe->boundValues()				)
	, _optionsDotJASP(		duplicateMe->_optionsDotJASP			)
	, _results(				duplicateMe->_results					)
	, _resultsMeta(			_results.get(".meta", Json::arrayValue)	)
	, _imgResults(			duplicateMe->_imgResults				)
	, _userData(			duplicateMe->_userData					)
	, _imgOptions(			duplicateMe->_imgOptions				)
	, _progress(			duplicateMe->_progress					)
	, _id(					id										)
	, _name(				duplicateMe->_name						)
	, _qml(					duplicateMe->_qml						)
	, _titleDefault(		duplicateMe->_titleDefault				)
	, _title("Copy of "+	duplicateMe->_title						)
	, _rfile(				duplicateMe->_rfile						)
	, _isDuplicate(			true									)
	, _version(				duplicateMe->_version					)
	, _moduleData(			duplicateMe->_moduleData				)
	, _dynamicModule(		duplicateMe->_dynamicModule				)
	, _codedAnalysisEntry(	duplicateMe->_codedAnalysisEntry		)
	, _helpFile(			duplicateMe->_helpFile					)
	, _rSources(			duplicateMe->_rSources					)
{
	initAnalysis();
}

void Analysis::initAnalysis()
{
	watchQmlForm();
	connect(&_QMLFileWatcher,	&QFileSystemWatcher::fileChanged,			this,	&Analysis::analysisQMLFileChanged,	Qt::UniqueConnection);
	connect(this,				&Analysis::createFormWhenYouHaveAMoment,	this,	&Analysis::createForm,				Qt::QueuedConnection);


	bool isNewAnalysis = optionsFromJASPFile().size() == 0;

	if(!_isDuplicate && isNewAnalysis)
		_status = Empty;

}

Analysis::~Analysis()
{
	if(form())
		destroyForm();

	if(DataSetPackage::pkg() && DataSetPackage::pkg()->hasDataSet())
		for(const std::string & col : computedColumns())
			emit requestComputedColumnDestruction(col);
}

void Analysis::clearOptions()
{
	_boundValues.clear();
}

bool Analysis::checkAnalysisEntry()
{
	/*
		This function is necessary because we have multiple datastructures representing the same data...

		The `RibbonModel` has its own copy and `Analysis` points there.
		We also have DynamicModule(s) which contains the original data as loaded from Description.qml and the Ribbon* objects are created based on that.
		
		This might mean that when a development module is installed the Analysis still has a pointer to the old RibbonButton/Model info and must be updated.
		Same for when Description.qml was changed and thus reloaded.

		To be able to find the right pointer we store a textual representation of which module+analysis is meant in Analysis::_codedAnalysisEntry.

		This function then corrects the pointers when necessary
	*/

	try
	{
		if(_codedAnalysisEntry == "" || !_dynamicModule)
			Modules::ModuleException("???", "No coded reference stored or _dynamicModule == nullptr...");

		_moduleData = _dynamicModule->retrieveCorrespondingAnalysisEntry(_codedAnalysisEntry);

		bool updateTitleToDefault = _title == _titleDefault;

		_titleDefault = _moduleData->title();

		if(updateTitleToDefault)
			setTitle(_titleDefault);

		bool qmlFileChanged = _lastQmlFormPath != qmlFormPath(false, true);

		_lastQmlFormPath = qmlFormPath(false, true);

		if(qmlFileChanged)
		{
			Log::log() << "Analysis::checkAnalysisEntry() has a changed qml file path, calling watchQmlForm()" << std::endl;
			watchQmlForm();

			reloadForm();
		}

		return true;
	}
	catch (Modules::ModuleException & e)
	{
		Log::log() << "Analysis::checkAnalysisEntry() had a problem: " << e.what() << std::endl;

		_moduleData = nullptr;
		if(_QMLFileWatcher.files().size())
			_QMLFileWatcher.removePaths(_QMLFileWatcher.files());
		return false;
	}
}

void Analysis::setTitle(const std::string& title)
{
	if (_title != title)
	{
		_title = title;
		if(_title == "")
			_title = _titleDefault;

		_results["title"] = _title;

		emit titleChanged();
	}
}

void Analysis::abort()
{
	setStatus(Aborting);
}

void Analysis::remove()
{
	abort();
	if (form())
		form()->cleanUpForm();
}


void Analysis::setResults(const Json::Value & results, Status status, const Json::Value & progress)
{
	_results		= results;
	_progress		= progress;
	_resultsMeta	= _results.get(".meta", Json::arrayValue);

	setStatus(status);

	emit resultsChangedSignal(this);

	processResultsForDependenciesToBeShown();

	if(_status == Analysis::Complete)
		checkForRSources();

	_wasUpgraded = false;
}

void Analysis::exportResults()
{
	emit Analyses::analyses()->analysesExportResults();
}

void Analysis::run()
{
	Log::log() << "Analysis::run() for " << title() << "(" << id() << ")" << std::endl;
	setStatus(Empty);
}

void Analysis::refresh()
{
	TempFiles::deleteAll(int(_id));
	run();

	emit refreshTableViewModels();
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
	std::string name = _imgOptions.get("name", "").asString();
	_imgResults = results;

	if (name != "")
	{
		setEditOptionsOfPlot(name, results["editOptions"]);

		if (_imgResults.get("resized", false).asBool() && !_imgResults.get("error", true).asBool())
			updatePlotSize(_imgOptions["name"].asString(), _imgResults.get("width", -1).asInt(), _imgResults.get("height", -1).asInt(), _results);
	}
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

void Analysis::imagesRewritten(const Json::Value & results)
{
	setResults(results, Analysis::Complete);
	emit resultsChangedSignal(this);
	emit imageChanged();

}

Analysis::Status Analysis::parseStatus(std::string name)
{
	if		(name == "empty")			return Analysis::Empty;
	else if (name == "initializing")	return Analysis::Empty;		//For backwards compatibility ?
	else if (name == "waiting")			return Analysis::Running;	//For backwards compatibility
	else if (name == "running")			return Analysis::Running;
	else if (name == "runningImg")		return Analysis::RunningImg;
	else if (name == "complete")		return Analysis::Complete;
	else if (name == "RewriteImgs")		return Analysis::RewriteImgs;
	else if (name == "validationError")	return Analysis::ValidationError;
	else if (name == "aborted")			return Analysis::Aborted;
	else if (name == "SaveImg")			return Analysis::SaveImg;
	else if (name == "EditImg")			return Analysis::EditImg;
	else								return Analysis::FatalError;
}

void Analysis::createForm(QQuickItem* parentItem)
{
	AnalysisBase::createForm(parentItem);

	if (_analysisForm)
	{
		connect(this,					&Analysis::rSourceChanged,			_analysisForm,	&AnalysisForm::rSourceChanged				);
		connect(this,					&Analysis::refreshTableViewModels,	_analysisForm,	&AnalysisForm::refreshTableViewModels		);
		connect(this, 					&Analysis::titleChanged,			_analysisForm,	&AnalysisForm::titleChanged					);
		connect(this,					&Analysis::needsRefreshChanged,		_analysisForm,	&AnalysisForm::needsRefreshChanged			);
	}
}

void Analysis::destroyForm()
{
	AnalysisBase::destroyForm();

	if (_analysisForm)
		_optionsDotJASP = _boundValues;  //So that we can later reload the controls to what we currently have
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
	case Analysis::RunningImg:		return "runningImg";
	case Analysis::Complete:		return "complete";
	case Analysis::Aborted:			return "aborted";
	case Analysis::Aborting:		return "aborting";
	case Analysis::SaveImg:			return "SaveImg";
	case Analysis::EditImg:			return "EditImg";
	case Analysis::RewriteImgs:		return "RewriteImgs";
	case Analysis::ValidationError:	return "validationError";
	case Analysis::FatalError:		return "fatalError";
	default:						return "?????";
	}
}

Json::Value Analysis::asJSON(bool withRSource) const
{
	Json::Value analysisAsJson = Json::objectValue;

	analysisAsJson["id"]			= int(_id);
	analysisAsJson["name"]			= _name;
	analysisAsJson["title"]			= _title;
	analysisAsJson["titleDef"]		= _titleDefault;
	analysisAsJson["rfile"]			= _rfile;
	analysisAsJson["progress"]		= _progress;
	analysisAsJson["version"]		= _version.asString();
	analysisAsJson["results"]		= _results;
	analysisAsJson["status"]		= statusToString(_status);
	analysisAsJson["options"]		= boundValues();
	analysisAsJson["userdata"]		= userData();
	analysisAsJson["dynamicModule"] = _moduleData->asJsonForJaspFile();

	if (withRSource)
		analysisAsJson["rSources"]	= rSources();

	Log::log() << "Analysis::asJSON():\n" << analysisAsJson.toStyledString() << std::endl;

	return analysisAsJson;
}

// This method tries to find the parent keys in _boundValues Json object
// If found, it sets the path to this reference to parentNames and returns a reference of the sub Json object
Json::Value& Analysis::_getParentBoundValue(const QVector<JASPControl::ParentKey>& parentKeys, QVector<std::string>& parentNames, bool& found, bool createAnyway)
{
	found = (parentKeys.size() == 0);
	Json::Value* parentBoundValue = &_boundValues;

	// A parentKey has 3 properties: <name>, <key> and <value>: it assumes that the json boundValue is an abject, one of its member is <name>,
	// whom value is an array of json objects. These objects have a member <key>, and one of them has for value <value>.
	// if there are several parentKeys, it repeats this operation
	//
	// {
	//		anOptionName : "one"
	//		...
	//		<name>: [
	//			{
	//				<key>: "anothervalue"
	//				anotherKey: "xxx"
	//			},
	//			{
	//				<key>: "<value>"
	//				anotherKey: "yyy" // parentBoundValue gets a reference to this Json object
	//			}
	//		]
	// }

	for (const auto& parent : parentKeys)
	{
		found = false;
		if (createAnyway && !parentBoundValue->isMember(parent.name))	(*parentBoundValue)[parent.name] = Json::Value(Json::arrayValue);

		if (parentBoundValue->isMember(parent.name))
		{
			Json::Value& parentBoundValues = (*parentBoundValue)[parent.name];
			if (!parentBoundValues.isNull() && parentBoundValues.isArray())
			{
				for (Json::Value & boundValue : parentBoundValues)
				{
					if (boundValue.isMember(parent.key))
					{
						Json::Value &val = boundValue[parent.key];
						// The value can be a string or an array of strings (for interaction terms)
						if (val.isString() && parent.value.size() > 0)
						{
							if (val.asString() == parent.value[0])	found = true;
						}
						else if (val.isArray() && val.size() == parent.value.size())
						{
							found = true;
							size_t i = 0;
							for (const Json::Value& compVal : val)
							{
								if (!compVal.isString() || compVal.asString() != parent.value[i]) found = false;
								i++;
							}
						}
						if (found)
						{
							parentBoundValue = &boundValue;
							parentNames.append(parent.name);
							break;
						}
					}
				}

				if (!found && createAnyway)
				{
					Json::Value row(Json::objectValue);
					if (parent.value.size() == 1)
						row[parent.key] = parent.value[0];
					else
					{
						Json::Value newValue(Json::arrayValue);
						for (size_t i = 0; i < parent.value.size(); i++)
							newValue.append(parent.value[i]);
						row[parent.key] = newValue;
					}
					parentBoundValues.append(row);
					parentBoundValue = &(parentBoundValues[parentBoundValues.size() - 1]);
					found = true;
				}
			}
		}
	}

	return *parentBoundValue;
}

bool Analysis::setBoundValue(const std::string &name, const Json::Value &value, const Json::Value &meta, const QVector<JASPControl::ParentKey>& parentKeys)
{
	bool found = false;
	QVector<std::string> parents;
	Json::Value& parentBoundValue = _getParentBoundValue(parentKeys, parents, found, true);
	Json::Value copyPBV				= parentBoundValue;

	if (found && parentBoundValue.isObject())
	{
		parentBoundValue[name] = value;

		if ((meta.isObject() || meta.isArray()) && meta.size() > 0)
		{
			Json::Value* metaBoundValue = &_boundValues[".meta"];
			for (const std::string& parent : parents)
				metaBoundValue = &((*metaBoundValue)[parent]);
			(*metaBoundValue)[name] = meta;
		}
	}

	return copyPBV != parentBoundValue;
}

bool Analysis::setBoundValues(const Json::Value &boundValues)
{
	bool changed = _boundValues != boundValues;
	_boundValues = boundValues;

	return changed;
}

const Json::Value &Analysis::boundValue(const std::string &name, const QVector<JASPControl::ParentKey> &parentKeys)
{
	bool found = false;
	QVector<std::string> parentNames;
	Json::Value& parentBoundValue = _getParentBoundValue(parentKeys, parentNames, found);


	if (found && !parentBoundValue.isNull() && parentBoundValue.isObject())	return parentBoundValue[name];
	else																	return Json::Value::null;
}

void Analysis::checkDefaultTitleFromJASPFile(const Json::Value & analysisData)
{
	//Lets make sure the title changes if the default changed compared with last time. (and the user didnt change it manually of course)
	std::string oldTitleDefault = analysisData.get("titleDef", _titleDefault).asString();

	if(_title == oldTitleDefault && _titleDefault != oldTitleDefault)
		_title = _titleDefault;

	_oldVersion		= analysisData.get("preUpgradeVersion", _results.get("version", AppInfo::version.asString())).asString();
}

void Analysis::loadResultsUserdataAndRSourcesFromJASPFile(const Json::Value & analysisData)
{
	Log::log() << "Now loading userdata results and R Sources for analysis " << _name << " from file." << std::endl;
	setUserData(analysisData["userdata"]);
	setResults(analysisData["results"], _status);
	setRSources(analysisData["rSources"]);

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

	Log::log() << "Analysis " << title() << " (" << id() << ") changes status from: " << statusToString(_status);

	//Make sure old notes etc aren't lost on table/plot-renames, see: https://github.com/jasp-stats/jasp-test-release/issues/469
	if(_status == Analysis::Complete)									storeUserDataEtc();
	if( status == Analysis::Complete && _status == Analysis::Running)	fitOldUserDataEtc();

	if (status == Analysis::Running && needsRefresh())
	{
		bool neededRefresh = needsRefresh();

		TempFiles::deleteList(TempFiles::retrieveList(_id));
		setVersion(AppInfo::version, true);

		_moduleVersion = _dynamicModule->version();

		if(neededRefresh != needsRefresh())
			emit needsRefreshChanged();
	}

	_status = status;

	Log::log(false) << " to: " << statusToString(_status) << std::endl;

	emit statusChanged(this);
}

void Analysis::boundValueChangedHandler()
{
	incrementRevision(); // To make sure we always process all changed options we increment the revision whenever anything changes

	Log::log() << "Option changed for analysis '" << name() << "' and id " << id() << ", revision incremented to: " << _revision << std::endl;

	if (_refreshBlocked)
		return;

	if (form() && (form()->hasError() || !form()->runOnChange()))
		return;

	run();
}

void Analysis::requestComputedColumnCreationHandler(const std::string& columnName)
{
	ComputedColumn *result = requestComputedColumnCreation(columnName, this);

	if (result)
		addOwnComputedColumn(columnName);
}

void Analysis::requestComputedColumnDestructionHandler(const std::string& columnName)
{
	emit requestComputedColumnDestruction(columnName);

	removeOwnComputedColumn(columnName);
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

std::set<std::string> Analysis::usedVariables()
{
	if (form())	return form()->usedVariables();

	return {};
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
	case performType::run:			setStatus(Analysis::Running);	break;
	case performType::saveImg:
	case performType::editImg:
	case performType::rewriteImgs:	setStatus(Analysis::RunningImg); break;
	default:														break;
	}

	Json::Value json = Json::Value(Json::objectValue);

	json["typeRequest"]			= engineStateToString(engineState::analysis);
	json["id"]					= int(id());
	json["perform"]				= performTypeToString(perform);
	json["revision"]			= revision();
	json["rfile"]				= _moduleData == nullptr ? rfile() : "";
	json["dynamicModuleCall"]	= _moduleData == nullptr ? "" : _moduleData->getFullRCall();
	json["resultsFont"]			= PreferencesModel::prefs()->resultFont().toStdString();

	if (!isAborted())
	{
		json["name"]			= name();
		json["title"]			= title();

		bool imgP = perform == performType::saveImg || perform == performType::editImg;
		if (imgP)	json["image"]		= imgOptions();

		json["options"]		= _boundValues;
	}

	return json;
}

void Analysis::emitDuplicationSignals()
{
	emit resultsChangedSignal(this);
	emit titleChanged();
}

QString	Analysis::fullHelpPath(QString helpFileName)
{
	return dynamicModule()->helpFolderPath() + helpFileName;
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

Json::Value Analysis::editOptionsOfPlot(const std::string & uniqueName, bool emitError)
{
	Json::Value editOptions;

	if(!_editOptionsOfPlot(_results, uniqueName, editOptions))
	{
		if (emitError)
			MessageForwarder::showWarning(tr("Could not find edit options of plot %1 so plot editing will not work...").arg(tq(uniqueName)));
		editOptions = Json::nullValue;
	}

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

bool Analysis::readyToCreateForm() const
{
	return dynamicModule() && dynamicModule()->readyForUse();
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

Json::Value Analysis::rSources() const
{
	Json::Value result(Json::objectValue);

	for (const auto& pair : _rSources)
		result[pair.first] = pair.second;

	return result;
}

void Analysis::storeUserDataEtc()
{
	if(!needsRefresh())
			return;

	_oldUserData = _userData;
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
		if(module() == "ANOVA" && Version(_oldVersion) < Version("0.12"))
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

bool Analysis::isWaitingForModule()
{
	//if moduleData == nullptr we might still be waiting for the module to be reloaded after replacement. Because it doesnt know which Analyses it contains yet.
	return !dynamicModule()->readyForUse();
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

	if (_analysisForm) _analysisForm->setHasVolatileNotes(checkForVolatileNotes(_userData));
}

void Analysis::setRSources(const Json::Value &rSources)
{
	_rSources.clear();
	if (rSources.isNull() || !rSources.isObject()) return;

	for (const std::string &sourceName : rSources.getMemberNames())
		_rSources[sourceName] = rSources[sourceName];
}

void Analysis::setDynamicModule(Modules::DynamicModule * module)
{
	Log::log() << "Replacing module connected to analysis " << title() << " (" << id() << ") for module " << module->name() << std::endl;
	if(_dynamicModule != module)
	{
		_dynamicModule = module;
		emit dynamicModuleChanged();
	}

	checkAnalysisEntry();
}

void Analysis::watchQmlForm()
{
	if(PreferencesModel::prefs()->developerMode())
	{
		QString		filePath		= tq(Analysis::qmlFormPath(false, true));

		if(_QMLFileWatcher.files().size())
			_QMLFileWatcher.removePaths(_QMLFileWatcher.files());

		if(!_QMLFileWatcher.addPath(filePath))
			Log::log()  << "Analysis::watchQmlForm, could not watch: " << filePath << std::endl;
	}
}


void Analysis::reloadForm()
{
	clearOptions();

	QQuickItem* parentForm = nullptr;

	if(readyToCreateForm())
	{
		emit emptyQMLCache();
		emit createFormWhenYouHaveAMoment();		//Give Qml a moment to clean up
	}
}

void Analysis::analysisQMLFileChanged()
{
	Log::log() << "Analysis::analysisQMLFileChanged() for " << name() << " (" << id() << ")" << std::endl;
	
	if(form() && form()->formCompleted())	reloadForm();
	else if(qmlError() != "")				createForm(); //Last time it failed apparently
	else
		Log::log() << "Form (" << form() << ") wasn't complete " << ( form() ? std::to_string(form()->formCompleted()) : " because there was no form...") << " yet, and also did not have a QML error set yet, so ignoring it." << std::endl;
}

void Analysis::checkForRSources()
{
	if(!_results.isMember(".meta"))
	{
		clearRSources();
		return;
	}

	//First check meta for qmlSources and collections
	std::set<std::string> sourceIDs, isCollection;

	std::function<void(Json::Value & meta)> findNewSource = [&](Json::Value & meta) -> void
	{
		if(meta.isArray())
			for(Json::Value & entry : meta)
				findNewSource(entry);
		else if(meta.isObject())
		{
			//Here we collect the meta's names for collections and qmlSources
			if(meta.isMember("type"))
			{
					if(		meta["type"].asString() == "qmlSource")		sourceIDs.insert(	meta["name"].asString());
					else if(meta["type"].asString() == "collection")	isCollection.insert(meta["name"].asString());
			}

			if(meta.isMember("meta")) //means there is an array of more meta below there
				findNewSource(meta["meta"]);
		}
	};

	findNewSource(_results[".meta"]);

	//Then uses the found collections and qmlSources to create the new list of qmlSources
	std::map<std::string, Json::Value> newSources;

	std::function<void(Json::Value & meta)> collectSources = [&](Json::Value & results) -> void
	{
		if(results.isArray())
			for(Json::Value & entry : results)
				findNewSource(entry);

		else if(results.isObject())
		{
			if(results.isMember("name") && sourceIDs.count(results["name"].asString()) > 0)
				newSources[results["sourceID"].asString()] = results["json"]; //We take the json from this qmlSource as that is the value we want

			for(const std::string & memberName : results.getMemberNames())
				if(sourceIDs.count(memberName) > 0)
					newSources[results[memberName]["sourceID"].asString()] = results[memberName]["json"];

				else if(isCollection.count(memberName) > 0 && results[memberName].isMember("collection")) //Checking for "collection" is to avoid stupid crashes but shouldnt really be necessary anyhow
					collectSources(results[memberName]["collection"]);
		}
	};

	collectSources(_results);
	//And then calculate the delta
	std::set<std::string> removeAfterwards;

	for(auto & sourceJson : _rSources)
		// The sourceIDs come from the meta values, newSources from the results
		// If a result of a source does not change, only its meta value is send, not its result.
		// So only when a source does not exist in the meta values, it can be removed
		if(sourceIDs.count(sourceJson.first) == 0)
		{
			removeAfterwards.insert(sourceJson.first);
			sourceJson.second = Json::nullValue;
			emit rSourceChanged(tq(sourceJson.first));
		}

	for(auto & newOptionJson : newSources)
	{
		//Make sure to only update rSource if it changes
		if(!_rSources.count(newOptionJson.first) ||  _rSources[newOptionJson.first] != newOptionJson.second)
		{
			_rSources[newOptionJson.first] = newOptionJson.second;
			emit rSourceChanged(tq(newOptionJson.first));
		}
	}

	for(const std::string & removeThis : removeAfterwards)
		_rSources.erase(removeThis);
}

void Analysis::clearRSources()
{
	for(auto & optionJson : _rSources)
	{
		optionJson.second = Json::nullValue;
		emit rSourceChanged(tq(optionJson.first));
	}

	_rSources.clear();
}

std::string Analysis::qmlFormPath(bool addFileProtocol, bool ignoreReadyForUse) const
{
	if(!ignoreReadyForUse && !dynamicModule()->readyForUse())
		return "";

	return (addFileProtocol ? "file:" : "") + (_moduleData != nullptr	?
				_moduleData->qmlFilePath()	:
				Dirs::resourcesDir() + "/" + module() + "/qml/"  + qml());
}
