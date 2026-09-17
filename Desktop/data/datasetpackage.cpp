//
// Copyright (C) 2018 University of Amsterdam
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

#include "datasetpackage.h"
#include "log.h"
#include "qutils.h"
#include <QThread>
#include "timers.h"
#include "utils.h"
#include "columnencoder.h"
#include "utilities/appdirs.h"
#include "engine/enginesync.h"
#include "gui/preferencesmodel.h"
#include "utilities/messageforwarder.h"
#include "databaseconnectioninfo.h"
#include "filtermodel.h"
#include "utilities/settings.h"
#include <ranges>
#include "variableinfo.h"
#include "fileevent.h"
#include "undostack.h"


DataSetPackage * DataSetPackage::_singleton = nullptr;

DataSetPackage::DataSetPackage(QObject * parent) : QObject(parent)
{
	if(_singleton) throw std::runtime_error("DataSetPackage can be constructed only once!");
	_singleton = this;
	//True init is done in setEngineSync!
	
	_db			= new DatabaseInterface(true);

	createWorkspace();
	
	connect(this, &DataSetPackage::isModifiedChanged,					this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::loadedChanged,						this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::currentFileChanged,					this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::folderChanged,						this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::isModifiedAfterAutoSaveChanged,		this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::currentFileChanged,					this, &DataSetPackage::nameChanged);
	connect(this, &DataSetPackage::dataModeChanged,						this, &DataSetPackage::onDataModeChanged);
	connect(this, &DataSetPackage::shownDataSetChanged,					this, &DataSetPackage::trackShownDataSet);
	
	connect(PreferencesModel::prefs(), &PreferencesModel::autoSaveAtAllChanged,			this, &DataSetPackage::handleAutoSavePrefChange);
	connect(PreferencesModel::prefs(), &PreferencesModel::autoSaveIntervalSecChanged,	this, &DataSetPackage::handleAutoSavePrefChange);

	connect(&_autoSaveTimer,			&QTimer::timeout, this, &DataSetPackage::handleAutoSave);
	
	_autoSaveTimer			.setSingleShot(false);
	handleAutoSavePrefChange();
}

DataSetPackage::~DataSetPackage() 
{ 
	_singleton = nullptr; 
}


void DataSetPackage::createWorkspace()
{
	assert(!_workspace);
	
	_workspace = new Workspace(this);
	
	_workspace->setShowRSyntax(PreferencesModel::prefs() ? PreferencesModel::prefs()->showRSyntaxInResults() : false);
	
	connectWorkspace();
	
	emit workspaceChanged();
}

DataSet * DataSetPackage::createDataSet()
{
	JASPTIMER_SCOPE(DataSetPackage::createDataSet);
	
	//The assumption here is that a new DataSet is needed. But not that anything else needs to be destroyed.
	
	if(!_workspace)
		createWorkspace();
	
	DataSet * dataSet = workspace()->createDataSet();
	
	//A brand new DataSet should start out with the configured default workspace empty values
	//(this also covers unittests, where there is no PreferencesModel).
	setDefaultWorkspaceEmptyValues();
		
	return dataSet;
}

void DataSetPackage::loadWorkspace(std::function<void(float)> progressCallback)
{
	if(workspace())
		deleteWorkspace(false); //no dbDelete necessary cause we just copied an old sqlite file here from the JASP file
	
	_db->close();
	stopEngines();

	// Whatever happens below (a failed DB migration, a corrupt column, ...) we must not leave the
	// application without engines. This guard restarts them on every exit path - success or
	// exception - so a failed load fails cleanly with a still-usable session instead of a dead app.
	struct EngineRestarter
	{
		DataSetPackage * pkg;
		~EngineRestarter() { try { pkg->restartEngines(); } catch(...) {} } // never throw during unwind
	} engineRestarter{ this };

	_db->load();
	_db->upgradeDBFromVersion(_jaspVersion);

	bool do019Upgrade = _jaspVersion < "0.19"; // A tweak needs to be made to the data as its loaded, see https://github.com/jasp-stats/jasp-desktop/pull/5367
	
	createWorkspace();
	
	workspace()->dbLoad(progressCallback, _jaspVersion);
	
	if (do019Upgrade)
	{
		// In 0.18.3 and before, there was a bug with the order of dataFilePath and description in the database.
		// dataFilePath was set empty and description has dataFilePath.
		if (dataSet()->dataFilePath().empty())
		{
			QFileInfo fileInfo(description());
			if (fileInfo.isFile())
				dataSet()->setDataFileQ(description());
		}
	}

	workspace()->initializeComputedColumns();
	workspace()->initializeComputedDatasets();

	refresh();
	// engines are restarted by engineRestarter on scope exit (also on the exception paths above)
}

void DataSetPackage::deleteWorkspace(bool dbDeletePlease)
{
	JASPTIMER_SCOPE(DataSetPackage::deleteWorkspace);
	
	if(dbDeletePlease)
		dbDelete();
	delete _workspace;
	_workspace = nullptr;

	//Always notify so QML can re-point its 'workspace' context (to null) instead of keeping a
	//dangling pointer to the just-deleted Workspace.
	emit shownDataSetChanged(nullptr);
	emit workspaceChanged();
}

void DataSetPackage::connectWorkspace()
{
	if(!workspace())
		return;
	
	Workspace		::connect(workspace(),	&Workspace::showWarning,						this,			&DataSetPackage::showWarning						);
	Workspace		::connect(workspace(),	&Workspace::showAnalysis,						this,			&DataSetPackage::showAnalysis						);
	Workspace		::connect(workspace(),	&Workspace::datasetChanged,						this,			&DataSetPackage::datasetChanged						);
	Workspace		::connect(workspace(),	&Workspace::somethingModified,					this,			&DataSetPackage::setModifiedFileMenu				);
	Workspace		::connect(workspace(),	&Workspace::dataModeChanged,					this,			&DataSetPackage::dataModeChanged					);
	Workspace		::connect(workspace(),	&Workspace::sendFilter,							this,			&DataSetPackage::sendFilter							);
	Workspace		::connect(workspace(),	&Workspace::sendFilterByName,					this,			&DataSetPackage::sendFilterByName					);
	Workspace		::connect(workspace(),	&Workspace::filtersCountChanged,				this,			&DataSetPackage::filtersCountChanged				);
	Workspace		::connect(workspace(),	&Workspace::shownFilterChanged,					this,			&DataSetPackage::shownFilterChanged					);
	Workspace		::connect(workspace(),	&Workspace::refreshAllAnalyses,					this,			&DataSetPackage::refreshAllAnalyses					);
	Workspace		::connect(workspace(),	&Workspace::shownDataSetChanged,				this,			&DataSetPackage::shownDataSetChanged				);	
	Workspace		::connect(workspace(),	&Workspace::dataSetCreated,						this,			&DataSetPackage::dataSetCreated					);	
	Workspace		::connect(workspace(),	&Workspace::dataSetRemoved,						this,			&DataSetPackage::dataSetRemoved					);	
	Workspace		::connect(workspace(),	&Workspace::runComputedColumn,					this,			&DataSetPackage::runComputedColumn					);	
	Workspace		::connect(workspace(),	&Workspace::runComputedDataSet,					this,			&DataSetPackage::runComputedDataSet					);	
	Workspace		::connect(workspace(),	&Workspace::checkForDependentAnalyses,			this,			&DataSetPackage::checkForDependentAnalyses			);	
	Workspace		::connect(workspace(),	&Workspace::emptyValuesChanged,					this,			&DataSetPackage::workspaceEmptyValuesChanged		);	

	DataSetPackage	::connect(this,			&DataSetPackage::filterByNameDone,				workspace(),	&Workspace::filterByNameDone						);
	
	
	emit shownDataSetChanged(nullptr);
	emit shownFilterChanged();
}


Filter * DataSetPackage::filter()
{
    return pkg()->workspace() && pkg()->workspace()->shownDataSet() ? pkg()->workspace()->shownDataSet()->shownFilter() : nullptr;
}

void DataSetPackage::setEngineSync(EngineSync * engineSync)
{
	_engineSync = engineSync;

	reset();
}

bool DataSetPackage::isThisTheSameThreadAsEngineSync()
{
	return	_engineSync && QThread::currentThread() == _engineSync->thread();
}

void DataSetPackage::reset(bool newDataSet)
{
	emit chooseColumn(-1); //Unselect any column in ColumnModel
	
	deleteWorkspace();

	if(newDataSet)	
		createDataSet();
	
	_archiveVersion				= Version();
	_jaspVersion				= Version();
	_analysesHTML				= QString();
	_analysesData				= Json::arrayValue;
	_warningMessage				= std::string();
	_hasAnalysesWithoutData		= false;
	_analysesHTMLReady			= false;
	_isJaspFile					= false;

	setLoaded(false);
	setModified(false);
	setCurrentFile("");
}

///This function assumes there should afterwards be only 1 DataSet!
void DataSetPackage::generateEmptyData()
{
	bool wasAlreadyLoaded = isLoaded();

	if(workspace())
		deleteWorkspace();
	createWorkspace();
	
	DataSet * newSet = dataSet() ? dataSet() : createDataSet();
	
	newSet->setColumnCount(1);
	newSet->setRowCount(1, false);
	
	newSet->column(0)->initFromLookups(newSet->freeNewColumnName(0), 1, [](size_t){return "";}, [](size_t){return "";}, "", columnType::scale, {}, thresholdScale(), orderByValueByDefault());
	
	setModified(false);
	
	if(!wasAlreadyLoaded)
	{
		emit newDataLoaded();
	}
	
	newSet->resetAllFilters();
	newSet->setDataFileSynch(false);
	
	if(workspace()->shownDataSet() != newSet)
		workspace()->setShownDataSet(newSet);
	else
		workspace()->refresh();
}

void DataSetPackage::onDataModeChanged(bool dataMode)
{
	if(workspace())
		workspace()->setDataMode(dataMode);
}

void DataSetPackage::setModified(bool value)
{
	if ((!value || _isLoaded || _hasAnalysesWithoutData) && value != _isModified)
	{
		_isModified = value;
		emit isModifiedChanged();
	}
	
	setModifiedAfterAutoSave(_isModified);
}

void DataSetPackage::setModifiedAfterAutoSave(bool value)
{
	if (value != _isModifiedAfterAutoSave)
	{
		_isModifiedAfterAutoSave = value;
		emit isModifiedAfterAutoSaveChanged();
	}
}


void DataSetPackage::handleAutoSave()
{
	if(_isModifiedAfterAutoSave)				
		emit makeAnAutoSave();
	
	else if(FileEvent::autoSaveExists() && _isModified)
			Utils::touch(fq(FileEvent::pathTmp()));
}


void DataSetPackage::setLoaded(bool loaded)
{
	if(loaded == _isLoaded)
		return;

	_isLoaded						= loaded;

	emit loadedChanged();
}

QString DataSetPackage::description() const
{
	return tq(dataSet() ? dataSet()->description() : "");
}

void DataSetPackage::setDescription(const QString &description)
{
	if (!dataSet()) return;
	
	dataSet()->setDescription(fq(description));

	emit descriptionChanged();
}

void DataSetPackage::prepareForLanguageChange()
{
	_waitingForLanguageChange = true; //Dont accept changes while the interface changes
}

void DataSetPackage::languageChangeDone()
{
	_waitingForLanguageChange = false; //Dont accept changes while the interface changes

	if(dataSet())
		dataSet()->refresh();
}

void DataSetPackage::handleAutoSavePrefChange()
{
	_autoSaveTimer.setInterval(1000 * (PreferencesModel::prefs() ? PreferencesModel::prefs()->autoSaveIntervalSec() : 1));
	
	if(PreferencesModel::prefs() && _autoSaveTimer.isActive() != PreferencesModel::prefs()->autoSaveAtAll())
	{
		if(!PreferencesModel::prefs()->autoSaveAtAll())		
			_autoSaveTimer.stop();
		else
			_autoSaveTimer.start();
	}
}


void DataSetPackage::refreshColumn(QString columnName)
{
	if(dataSet() && dataSet()->column(columnName))
	{
		dataSet()->column(columnName)->refresh();
		refresh(); //Hopefully trigger sortfilterproxymodel model reconstruction
	}
}


void DataSetPackage::columnWasOverwritten(const std::string & columnName, const std::string &)
{
	if(dataSet())
		dataSet()->emitColumnChanged(tq(columnName));
}


void DataSetPackage::refresh()
{
	if(!dataSet())
		return;
	
	dataSet()->refresh();
}




void DataSetPackage::stopEngines()
{
	if(EngineSync::singleton()) //During testing this may be false
		EngineSync::singleton()->stopEngines();
}

void DataSetPackage::restartEngines()
{
	if(EngineSync::singleton()) //During testing this may be false
		EngineSync::singleton()->restartEngines();
}



void DataSetPackage::dbDelete()
{
	JASPTIMER_SCOPE(DataSetPackage::dbDelete);

	if(!workspace())
		return;

	//NOTE (deliberate semantics): this is a FULL teardown (New / close file), so it permanently purges
	//*every* dataset from SQLite, not only the shown one. Callers must pair this with a full
	//Analyses/UI reset so nothing keeps a reference to the purged datasets. Single-dataset deletion is
	//a different operation and does NOT go through here (see Workspace::deleteShownDataSet).
	DataSets sets = workspace()->dataSets();
	for (DataSet * ds : sets)
		if (ds && ds->id() != -1)
			ds->dbDelete();
}

int DataSetPackage::thresholdScale()
{
	//In unittests there is no PreferencesModel, so fall back to the configured default (10) instead of a hardcoded value.
	return PreferencesModel::prefs() ? PreferencesModel::prefs()->thresholdScale() : Settings::value(Settings::THRESHOLD_SCALE).toInt();
}

int DataSetPackage::orderByValueByDefault()
{
	return PreferencesModel::prefs() ? int(PreferencesModel::prefs()->orderByValueByDefault()) : true;
}

void DataSetPackage::resetVariableTypes()
{
	if(workspace())
		for(DataSet * dataSet : workspace()->dataSets())
			dataSet->resetVariableTypes(thresholdScale());
}

bool DataSetPackage::workspaceShowRSyntax() const
{
	return workspace() ? workspace()->showRSyntax() : (PreferencesModel::prefs() ? PreferencesModel::prefs()->showRSyntaxInResults() : false);
}


void DataSetPackage::setDataSetEmptyValues(const stringset &emptyValues, bool reset)
{
	if (!workspace()) 
		return;
	
	
	for(DataSet * dataSet : workspace()->dataSets())
		dataSet->setEmptyValuesFromStrings(emptyValues);
	
	if(reset)	
		refresh();
	
	emit workspaceEmptyValuesChanged();
}

void DataSetPackage::setDefaultWorkspaceEmptyValues()
{
	stringvec prefs;

	if (PreferencesModel::prefs())
		prefs = fq(PreferencesModel::prefs()->emptyValues());
	else if (Settings::value(Settings::EMPTY_VALUES_LIST).isValid())
	{
		// In unittests there is no PreferencesModel, but we still want to apply the configured
		// default empty values (Settings::value(EMPTY_VALUES_LIST) returns them in test mode too).
		QStringList items = Settings::value(Settings::EMPTY_VALUES_LIST).toString().split("|");
		std::set<QString> ordered(items.begin(), items.end());
		prefs = fq(QStringList(ordered.begin(), ordered.end()));
	}

	setDataSetEmptyValues(stringset(prefs.begin(), prefs.end()));
}

void DataSetPackage::setWorkspaceShowRSyntax(bool show)
{
	if (!workspace() || workspace()->showRSyntax() == show) 
		return;

	workspace()->setShowRSyntax(show);

	setModified(true);
}


void DataSetPackage::setCurrentFile(QString currentFile)
{
	if (_currentFile == currentFile)
		return;

	_currentFile = currentFile;
	emit currentFileChanged();

	QFileInfo	file(_currentFile);
	QUrl		url(_currentFile);

#ifdef _WIN32
	setFolder(file.exists() ? file.absolutePath().replace('/', '\\')	: url.isValid() ? "OSF" : "");
#else
	setFolder(file.exists() ? file.absolutePath()						: url.isValid() ? "OSF" : "");
#endif
}

void DataSetPackage::setFolder(QString folder)
{
	//Remove the last part if it is the name of the file regardless of extension
	QString _name	= name();
	int		i		= _name.size();
	for(; i < folder.size(); i++)
		if(folder.right(i).startsWith(_name))
		{
			folder = folder.left(folder.size() - i);
			break;
		}
#ifdef _WIN32
		else if(folder.right(i).contains('\\'))	break;
#else
		else if(folder.right(i).contains('/'))	break;
#endif

	if (_folder == folder)
		return;

	_folder = folder;
	emit folderChanged();
}

QString DataSetPackage::name() const
{
	QFileInfo	file(_currentFile);

	if(file.completeBaseName() != "")
		return file.completeBaseName();

	return "JASP";
}

bool DataSetPackage::dataMode() const
{
	return workspace() && workspace()->dataMode();
}

QString DataSetPackage::windowTitle() const
{
	QString name	= DataSetPackage::name(),
			folder	= DataSetPackage::folder();
	
#ifdef _WIN32
	if(folder.startsWith(AppDirs::examples().replace('/', '\\')))
#else
	if(folder.startsWith(AppDirs::examples()))
#endif
		folder = "";

	folder = folder == "" ? "" : "      (" + folder + ")";

	return name + (isModified() ? isModifiedAfterAutoSave() ? "*" : "* (autosaved)"  : "") + folder;
}


bool DataSetPackage::currentJaspFileIsNonSaveable() const
{
	return filePathIsNonSaveable(currentFile());
}

bool DataSetPackage::filePathIsNonSaveable(const QString & path) const
{
	QFileInfo fileDir(path);

	return fileDir.dir().absolutePath().startsWith(AppDirs::examples()) || fileDir.dir() == QDir(AppDirs::autoSaveDir());
}

void DataSetPackage::setAnalysesData(const Json::Value &analysesData)
{
	QString		previousASF					= analysesData.type() != Json::objectValue ? "" : tq(analysesData.get("autoSaveFileName", "").asString());
				_analysesData				= analysesData;
	QFileInfo	dataFile					( tq(dataSet() ? dataSet()->dataFilePath() : "") ),
				curFileI					( currentFile() );
	QString		dataFileName				= dataFile.fileName(),
				curFile						= currentFile(),
				autoSaveString				= curFile != "JASP" ? tr("%1 autosaved").arg(curFileI.fileName()) + "<br>" + tr("Full path: %1").arg("<code>"+curFileI.absoluteFilePath()+"</code>") : dataFileName == "" ? tr("Unsaved workspace") : tr("Unsaved workspace of datafile %1").arg(dataFileName);

	_analysesData["autoSaveDescription"]	= fq(autoSaveString);
	_analysesData["autoSaveFileName"]		= fq(curFileI.exists() ? curFileI.fileName() : previousASF != "" ? previousASF : curFile != "" ? curFile : tr("Autosave"));
}


QString DataSetPackage::autoSavedFileName() const
{
	return tq(_analysesData.get("autoSaveFileName", fq(currentFile())).asString());
}

// This function can be called from a different thread then where the underlying value for isReady() is set, but I don't think a mutex or whatever is necessary here. What could go wrong with checking a boolean?
// Also this was already the case, so I'm not making things worse here...
void DataSetPackage::waitForExportResultsReady() 
{ 
	int maxSleepTime	= 10000,
		sleepTime		= 100,
		delay			= 0;
	
	while (!isReady())
	{
		if (delay > maxSleepTime)
			break;
		
		Utils::sleep(sleepTime);
		delay += sleepTime;
	}
	
	if(!isReady())
		Log::log() << "Results were not exported properly!" << std::endl; //Should we maybe create a dummy result that explains something went wrong with the upload? Should we abort saving? What is going on?
}


void DataSetPackage::checkDataSetForUpdates()
{
	if(!_workspace)
		return;

	_workspace->checkForUpdates();
}

//Editing by hand, and what that does to the external synching, is per-dataset bookkeeping and lives on
//DataSet: another dataset can be shown, and edited, in the meantime. These two only pass the question on
//to whichever dataset is shown, for the QML property and for everybody who thinks in terms of "the" data.
bool DataSetPackage::manualEdits() const
{
	return dataSet() && dataSet()->manualEdits();
}

void DataSetPackage::setManualEdits(bool newManualEdits)
{
	if(dataSet())
		dataSet()->setManualEdits(newManualEdits);
}

bool DataSetPackage::synchingExternally() const
{
	DataSet * ds = dataSet();

	return ds && ds->dataFileSynch() && (!ds->dataFilePath().empty() || ds->syncer().isDatabaseSyncing());
}

void DataSetPackage::setSynchingExternally(bool synchingExternally)
{
	DataSet * ds = dataSet();

	if(ds)
	{
		if(!synchingExternally)
			ds->syncer().stopFileSyncing();
		else
		{
			ds->syncer().startFileSyncing(tq(ds->dataFilePath()));

			//The data file is leading again, so the edits made by hand no longer block it.
			//Clearing the flag also makes sure a *next* manual edit can disable the synching again.
			if(ds->dataFileSynch())
				setManualEdits(false);
		}
	}

	emitSynchingExternallyChanged();
}

void DataSetPackage::setSynchingExternallyFriendly(bool synchingExternally)
{
	if(synchingExternally)
	{
		//There might not be a (usable) data file to synch with, for instance because the data was
		//entered or edited by hand. So let the user generate or find one first.
		if(!emit askUserForExternalDataFile())
			return;

		setSynchingExternally(true);
	}
	else if(dataSet() && dataSet()->dataFileSynch())
		setSynchingExternally(false);

	setModified(true); //Perhaps someone would like to save the fact that it should (not) be synchronized
}

void DataSetPackage::onUndoCleanChanged(bool clean)
{
	//Undone all the way back to the point where the data still matched the data file (see
	//DataSet::setDataFileSynch), so the edits that switched the synching off are gone and it can go
	//back on. Only when those edits were what switched it off, of course.
	if(clean && dataSet() && dataSet()->manualEdits() && dataSet()->synchTurnedOffByManualEdits())
		setSynchingExternally(true);
}

void DataSetPackage::emitSynchingExternallyChanged()
{
	emit synchingExternallyChanged(synchingExternally());
}

void DataSetPackage::trackShownDataSet()
{
	DataSet * shown = dataSet();

	if(_synchTrackedDataSet != shown)
	{
		if(_synchTrackedDataSet)
		{
			disconnect(_synchTrackedDataSet,				&DataSet::dataFileSynchChanged,	this,	&DataSetPackage::emitSynchingExternallyChanged);
			disconnect(_synchTrackedDataSet,				&DataSet::dataFileChanged,		this,	&DataSetPackage::emitSynchingExternallyChanged);
			disconnect(_synchTrackedDataSet,				&DataSet::manualEditsChanged,	this,	&DataSetPackage::manualEditsChanged);
			disconnect(_synchTrackedDataSet->undoStack(),	&QUndoStack::cleanChanged,		this,	&DataSetPackage::onUndoCleanChanged);
		}

		_synchTrackedDataSet = shown;

		if(_synchTrackedDataSet)
		{
			connect(_synchTrackedDataSet,					&DataSet::dataFileSynchChanged,	this,	&DataSetPackage::emitSynchingExternallyChanged);
			connect(_synchTrackedDataSet,					&DataSet::dataFileChanged,		this,	&DataSetPackage::emitSynchingExternallyChanged);
			connect(_synchTrackedDataSet,					&DataSet::manualEditsChanged,	this,	&DataSetPackage::manualEditsChanged);
			connect(_synchTrackedDataSet->undoStack(),		&QUndoStack::cleanChanged,		this,	&DataSetPackage::onUndoCleanChanged);
		}

		//Another dataset is shown, so both answers can be different now.
		emit manualEditsChanged();
	}

	emitSynchingExternallyChanged();
}

