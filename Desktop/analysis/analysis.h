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

#ifndef ANALYSIS_H
#define ANALYSIS_H

#include <boost/uuid/uuid.hpp>

#include "common.h"
#include "../Common/version.h"

#include "enginedefinitions.h"
#include "jaspcontrol.h"

#include <set>
#include <QObject>
#include "modules/dynamicmodules.h"
#include "data/datasetpackage.h"
#include "utilities/qutils.h"
#include "modules/upgrader/upgradechange.h"

class ComputedColumn;
class DataSet;
class AnalysisForm;

class Analysis : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString	name				READ nameQ											NOTIFY nameChanged				)
	Q_PROPERTY(QString	title				READ titleQ				WRITE setTitleQ				NOTIFY titleChanged				)
	Q_PROPERTY(QString	helpFile			READ helpFile										NOTIFY helpFileChanged			)
	Q_PROPERTY(QString	helpMD				READ helpMD											NOTIFY helpMDChanged			)
	Q_PROPERTY(bool		needsRefresh		READ needsRefresh									NOTIFY needsRefreshChanged		)
	///Volatile notes are coupled with results elements and might disappear when the name changes. Some attempt is made to salvage them on a refresh when needsRefresh is true!
	Q_PROPERTY(bool		hasVolatileNotes	READ hasVolatileNotes	WRITE setHasVolatileNotes	NOTIFY hasVolatileNotesChanged	)
	Q_PROPERTY(bool		optionsBound		READ optionsBound		WRITE setOptionsBound		NOTIFY optionsBoundChanged		)

	friend class Analyses;

	typedef std::map<std::string, std::set<std::string>> optionColumns;

public:

	enum Status { Empty, Running, RunningImg, Complete, Aborting, Aborted, ValidationError, SaveImg, EditImg, RewriteImgs, FatalError, Initializing, KeepStatus };
	void setStatus(Status status);
	static std::string statusToString(Status status);
	///This function transforms an analysisResultStatus to Analysis::Status so that the Analysis gets the correct status after returning from Engine
	static Analysis::Status analysisResultsStatusToAnalysisStatus(analysisResultStatus result);

	Analysis(size_t id, Analysis * duplicateMe);
	Analysis(size_t id, Modules::AnalysisEntry * analysisEntry, std::string title = "", std::string moduleVersion = "", Json::Value *data = nullptr);

	virtual ~Analysis();

	void				clearOptions();
	const Json::Value&	optionsFromJASPFile()		const	{ return _optionsDotJASP;	}

	const Json::Value&	boundValues()				const	{ return _boundValues;		}
	const Json::Value&	boundValue(const std::string& name, const QVector<JASPControl::ParentKey>& parentKeys = {});
	
	bool				setBoundValue(const std::string& name, const Json::Value& value, const Json::Value& meta, const QVector<JASPControl::ParentKey>& parentKeys = {});
	bool				setBoundValues(const Json::Value& boundValues);

	Q_INVOKABLE	QString	fullHelpPath(QString helpFileName);
	Q_INVOKABLE void	duplicateMe();

	bool needsRefresh()			const;
	bool wasUpgraded()			const { return _wasUpgraded; }
	bool isWaitingForModule();
	bool isDynamicModule()		const { return bool(_dynamicModule); }
	void setResults(		const Json::Value & results, analysisResultStatus	status, const Json::Value & progress = Json::nullValue) { setResults(results, analysisResultsStatusToAnalysisStatus(status), progress); }
	void setResults(		const Json::Value & results, Status					status, const Json::Value & progress = Json::nullValue);
	void imageSaved(		const Json::Value & results);
	void saveImage(			const Json::Value & options);
	void editImage(			const Json::Value & options);
	void imageEdited(		const Json::Value & results);
	void imagesRewritten(	const Json::Value & results);
	void rewriteImages();

	void setRFile(const std::string &file)				{ _rfile = file;								}
	void setUserData(Json::Value userData);
	void setVersion(Version version, bool resetWasUpgraded = false);
	void setRefreshBlocked(bool block)					{ _refreshBlocked = block;						}
	void incrementRevision()							{ _revision++;									}

	void setErrorInResults(const std::string & msg);


	Json::Value editOptionsOfPlot(const std::string & uniqueName, bool emitError = true);
	void		setEditOptionsOfPlot(const std::string & uniqueName, const Json::Value & editOptions);
	bool		checkAnalysisEntry();

	//getters
	const	Json::Value		&	results()			const	{ return _results;							}
	const	Json::Value		&	userData()			const	{ return _userData;							}
	const	std::string		&	name()				const	{ return _name;								}
	const	QString				nameQ()				const	{ return tq(_name);							}
	const	std::string		&	qml()				const	{ return _qml;								}
	const	Version			&	version()			const	{ return _version;							}
	const	std::string		&	title()				const	{ return _title;							}
			QString				titleQ()			const	{ return tq(_title);						}
	const	std::string		&	rfile()				const	{ return _rfile;							}
	const	std::string		&	module()			const	{ return _module;							}
			size_t				id()				const	{ return _id;								}
			Status				status()			const	{ return _status;							}
			QString				statusQ()			const	{ return tq(statusToString(_status));		}
			int					revision()			const	{ return _revision;							}
			bool				isRefreshBlocked()	const	{ return _refreshBlocked;					}
			QString				helpFile()			const	{ return _helpFile;							}
	const	Json::Value		&	imgOptions()		const	{ return _imgOptions;						}
	const	Json::Value		&	imgResults()		const	{ return _imgResults;						}
	Modules::DynamicModule	*	dynamicModule()		const	{ return _dynamicModule;					}
			AnalysisForm	*	form()				const	{ return _analysisForm;						}
			bool				isDuplicate()		const	{ return _isDuplicate;						}
			bool				hasVolatileNotes()	const	{ return _hasVolatileNotes;					}
			bool				utilityRunAllowed() const	{ return  isSaveImg() || isEditImg() || isRewriteImgs();									}
			bool				shouldRun()					{ return !isWaitingForModule() && ( utilityRunAllowed() || isEmpty() ) && optionsBound();	}
	const	Json::Value		&	resultsMeta()		const	{ return _resultsMeta;						}
	const	Json::Value			optionsMeta()		const	{ return _boundValues.get(".meta", Json::nullValue);	}
			QString				helpMD()			const;
			bool				optionsBound()		const	{ return _optionsBound;	}

			void		run();
			void		refresh();
			void		reload();
			void        exportResults();
			void		remove();

			Json::Value asJSON()		const;
			void		checkDefaultTitleFromJASPFile(	const Json::Value & analysisData);
			void		loadResultsUserdataFromJASPFile(const Json::Value & analysisData);
			Json::Value createAnalysisRequestJson();

	static	Status		parseStatus(std::string name);

	bool isEmpty()			const { return status() == Empty;		}
	bool isAborted()		const { return status() == Aborted;		}
	bool isSaveImg()		const { return status() == SaveImg;		}
	bool isRewriteImgs()	const { return status() == RewriteImgs;	}
	bool isEditImg()		const { return status() == EditImg;		}
	bool isRunningImg()		const { return status() == RunningImg;	}
	bool isFinished()		const { return status() == Complete || status() == ValidationError || status() == FatalError; }


	void initialized(AnalysisForm* form, bool isNewAnalysis);

	performType				desiredPerformTypeFromAnalysisStatus() const;
	std::string				qmlFormPath() const;

	std::set<std::string>	usedVariables();
	void					runScriptRequestDone(const QString& result, const QString& controlName);

	void					setUpgradeMsgs(const Modules::UpgradeMsgs & msgs);
	std::string				upgradeMsgsForOption(const std::string & name)		const;

	const QList<std::string>	& computedColumns()								const	{ return _computedColumns; }
	const Json::Value			& getRSource(const std::string& name)			const	{ return _rSources.count(name) > 0 ? _rSources.at(name) : Json::Value::null; }
	
signals:
	void				nameChanged();
	void				helpFileChanged();
	void				helpMDChanged();
	void				titleChanged();
	void				needsRefreshChanged();
	void				hasVolatileNotesChanged(bool hasVolatileNotes);

	void				sendRScript(			Analysis * analysis, QString script, QString controlName, bool whiteListedVersion);
	void				statusChanged(			Analysis * analysis);
	void				imageSavedSignal(		Analysis * analysis);
	void				imageEditedSignal(		Analysis * analysis);
	void				resultsChangedSignal(	Analysis * analysis);
	void				userDataChangedSignal(	Analysis * analysis);
	void				imageChanged();
	void				rSourceChanged(QString optionName);

	ComputedColumn *	requestComputedColumnCreation(		const std::string& columnName, Analysis * analysis);
	void				requestColumnCreation(				const std::string& columnName, Analysis *source, columnType type);
	void				requestComputedColumnDestruction(	const std::string& columnName);

	void				refreshTableViewModels();
	Q_INVOKABLE void	expandAnalysis();




	void optionsBoundChanged(bool optionsBound);

public slots:
	void					setName(std::string name);
	void					setNameQ(QString name) { setName(name.toStdString()); }
	void					setHelpFile(QString helpFile);
	void					setTitleQ(QString title);
	void					setTitle(std::string title) { setTitleQ(tq(title)); }
	void					setHasVolatileNotes(bool hasVolatileNotes);
	void					setDynamicModule(Modules::DynamicModule * module);
	void					emitDuplicationSignals();
	void					showDependenciesOnQMLForObject(QString uniqueName); //uniqueName is basically "name" in meta in results.
	void					setOptionsBound(bool optionsBound);
	void					boundValueChangedHandler();
	ComputedColumn *		requestComputedColumnCreationHandler(const std::string& columnName);
	void					requestColumnCreationHandler(const std::string&columnName, columnType colType)	{ requestColumnCreation(columnName, this, colType); }
	void					requestComputedColumnDestructionHandler(const std::string& columnName);


protected:
	void					abort();
	void					addOwnComputedColumn(const std::string& col)				{ _computedColumns.push_back(col); }
	void					removeOwnComputedColumn(const std::string& col)				{ _computedColumns.removeAll(col); }

private:
	void					processResultsForDependenciesToBeShown();
	bool					processResultsForDependenciesToBeShownMetaTraverser(const Json::Value & array);
	bool					_editOptionsOfPlot(const Json::Value & results, const std::string & uniqueName, Json::Value & editOptions);
	bool					_setEditOptionsOfPlot(Json::Value & results, const std::string & uniqueName, const Json::Value & editOptions);
	void					storeUserDataEtc();
	void					fitOldUserDataEtc();
	bool					updatePlotSize(const std::string & plotName, int width, int height, Json::Value & root);
	Modules::AnalysisEntry	*moduleData();
	void					checkForRSources();
	void					clearRSources();
	Json::Value&			_getParentBoundValue(const QVector<JASPControl::ParentKey>& parentKeys, bool& found, bool createAnyway = false);

protected:
	Status					_status			= Initializing;
	bool					_refreshBlocked	= false;

	Json::Value				_boundValues;


	///For backward compatibility: _optionsDotJASP = options from (old) JASP file.
	Json::Value				_optionsDotJASP = Json::nullValue,
							_results		= Json::nullValue,
							_resultsMeta	= Json::nullValue,
							_imgResults		= Json::nullValue,
							_userData		= Json::nullValue,
							_imgOptions		= Json::nullValue,
							_progress		= Json::nullValue,
							_oldUserData	= Json::nullValue,
							_oldMetaData	= Json::nullValue;
	std::string				_oldVersion		= "0";

private:
	size_t					_id,
							_counter		= 0;
	std::string				_module			= "dynamic",
							_name,
							_qml,
							_titleDefault,
							_title,
							_rfile,
							_showDepsName		= "",
							_moduleVersion		= "";
	bool					_isDuplicate		= false,
							_wasUpgraded		= false,
							_hasVolatileNotes	= false,
							_tryToFixNotes		= false,
							_optionsBound		= false;
	Version					_version;
	int						_revision		= 0;

	Modules::AnalysisEntry*	_moduleData		= nullptr;
	Modules::DynamicModule* _dynamicModule	= nullptr;
	AnalysisForm*			_analysisForm	= nullptr;
	QList<std::string>		_computedColumns;

	std::string				_codedReferenceToAnalysisEntry = "";
	QString					_helpFile;

	Modules::UpgradeMsgs	_msgs;

	std::map<std::string,
		Json::Value>		_rSources;
};

#endif // ANALYSIS_H
