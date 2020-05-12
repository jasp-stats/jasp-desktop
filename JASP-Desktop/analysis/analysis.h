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
#include "../JASP-Common/version.h"

#include "options/options.h"
#include "enginedefinitions.h"

#include <set>
#include <QObject>
#include "modules/dynamicmodules.h"
#include "data/datasetpackage.h"
#include "utilities/qutils.h"
#include "modules/upgrader/upgradechange.h"

class ComputedColumn;
class Analyses;
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



	typedef std::map<std::string, std::set<std::string>> optionColumns;

public:

	enum Status { Empty, Initing, Inited, Running, Complete, Aborting, Aborted, ValidationError, SaveImg, EditImg, RewriteImgs, FatalError, Initializing };
	void setStatus(Status status);
	static std::string statusToString(Status status);
	static Analysis::Status analysisResultsStatusToAnalysisStatus(analysisResultStatus result);

	Analysis(size_t id, Analysis * duplicateMe);
	Analysis(size_t id, std::string module, std::string name, std::string qml, std::string title, const Version &version, Json::Value *data);
	Analysis(size_t id, Modules::AnalysisEntry * analysisEntry, std::string title = "", std::string moduleVersion = "", Json::Value *data = nullptr);

	virtual ~Analysis();

	void				resetOptionsFromJASPFile()			{ _optionsDotJASP.clear();	}
	void				clearOptions();
	Options				*options()					const	{ return _options;			}
	const Json::Value&	optionsFromJASPFile()		const	{ return _optionsDotJASP;	}

	Q_INVOKABLE	QString	fullHelpPath(QString helpFileName);
	Q_INVOKABLE void	duplicateMe();

	bool needsRefresh()			const;
	bool isWaitingForModule()	const { return _moduleData == nullptr ? false : !_moduleData->dynamicModule()->readyForUse(); }
	bool isDynamicModule()		const { return _moduleData == nullptr ? false : _moduleData->dynamicModule() != nullptr; }
	void setResults(	const Json::Value & results, analysisResultStatus	status, const Json::Value & progress = Json::nullValue) { setResults(results, analysisResultsStatusToAnalysisStatus(status), progress); }
	void setResults(	const Json::Value & results, Status					status, const Json::Value & progress = Json::nullValue);
	void imageSaved(	const Json::Value & results);
	void saveImage(		const Json::Value & options);
	void editImage(		const Json::Value & options);
	void imageEdited(	const Json::Value & results);
	void rewriteImages();
	void imagesRewritten();

	void setRFile(const std::string &file)				{ _rfile = file;								}
	void setUserData(Json::Value userData);
	void setVersion(Version version, bool resetWasUpgraded = false);
	void setRefreshBlocked(bool block)					{ _refreshBlocked = block;						}
	void setUsesJaspResults(bool usesJaspResults)		{ _useJaspResults = usesJaspResults;			}
	void incrementRevision()							{ _revision++;									}

	void setErrorInResults(const std::string & msg);


	Json::Value editOptionsOfPlot(const std::string & uniqueName);
	void		setEditOptionsOfPlot(const std::string & uniqueName, const Json::Value & editOptions);
	bool		checkAnalysisEntry();

	//getters
	const	Json::Value		&	results()			const	{ return _results;						}
	const	Json::Value		&	meta()				const	{ return _meta;							}
	const	Json::Value		&	userData()			const	{ return _userData;						}
	const	std::string		&	name()				const	{ return _name;							}
	const	QString				nameQ()				const	{ return tq(_name);						}
	const	std::string		&	qml()				const	{ return _qml;							}
	const	Version			&	version()			const	{ return _version;						}
	const	std::string		&	title()				const	{ return _title;						}
			QString				titleQ()			const	{ return tq(_title);					}
	const	std::string		&	rfile()				const	{ return _rfile;						}
	const	std::string		&	module()			const	{ return _module;						}
			size_t				id()				const	{ return _id;							}
			bool				usesJaspResults()	const	{ return _useJaspResults;				}
			Status				status()			const	{ return _status;						}
			QString				statusQ()			const	{ return tq(statusToString(_status));	}
			int					revision()			const	{ return _revision;						}
			bool				isRefreshBlocked()	const	{ return _refreshBlocked;				}
			QString				helpFile()			const	{ return _helpFile;						}
			QString				helpMD()			const;
	const	Json::Value		&	imgOptions()		const	{ return _imgOptions;					}
	const	Json::Value		&	imgResults()		const	{ return _imgResults;					}
	Modules::DynamicModule	*	dynamicModule()		const	{ return _dynamicModule;				}
			AnalysisForm	*	form()				const	{ return _analysisForm;					}
			bool				isDuplicate()		const	{ return _isDuplicate;					}
			bool				hasVolatileNotes()	const	{ return _hasVolatileNotes;				}

			void		run();
			void		refresh();
			void		reload();
			void		rebind();
			void        exportResults();
	virtual void		abort();
			void		remove();

			Json::Value asJSON()		const;
			void		loadExtraFromJSON(Json::Value & options);
			Json::Value createAnalysisRequestJson();

	static	Status		parseStatus(std::string name);

	bool isEmpty()			const { return status() == Empty;		}
	bool isAborted()		const { return status() == Aborted;		}
	bool isSaveImg()		const { return status() == SaveImg;		}
	bool isRewriteImgs()	const { return status() == RewriteImgs;	}
	bool isEditImg()		const { return status() == EditImg;		}
	bool isInited()			const { return status() == Inited;		}
	bool isFinished()		const { return status() == Complete || status() == ValidationError || status() == FatalError; }


	void initialized(AnalysisForm* form, bool isNewAnalysis);

	performType				desiredPerformTypeFromAnalysisStatus() const;
	std::string				qmlFormPath() const;

	std::set<std::string>	usedVariables()																	{ return _options->usedVariables();					}
	std::set<std::string>	columnsCreated()																{ return _options->columnsCreated();				}
	void					removeUsedVariable(const std::string & var)										{ _options->removeUsedVariable(var);				}
	void					replaceVariableName(const std::string & oldName, const std::string & newName);
	void					runScriptRequestDone(const QString& result, const QString& controlName);

	void					setUpgradeMsgs(const Modules::UpgradeMsgs & msgs);
	std::string				upgradeMsgsForOption(const std::string & name) const;

signals:
	void				nameChanged();
	void				helpFileChanged();
	void				helpMDChanged();
	void				titleChanged();
	void				needsRefreshChanged();
	void				hasVolatileNotesChanged(bool hasVolatileNotes);

	void				sendRScript(			Analysis * analysis, QString script, QString controlName, bool whiteListedVersion);
	void				statusChanged(			Analysis * analysis);
	void				optionsChanged(			Analysis * analysis);
	void				imageSavedSignal(		Analysis * analysis);
	void				imageEditedSignal(		Analysis * analysis);
	void				resultsChangedSignal(	Analysis * analysis);
	void				userDataChangedSignal(	Analysis * analysis);

	ComputedColumn *	requestComputedColumnCreation(		QString columnName, Analysis * analysis);
	void				requestColumnCreation(				QString columnName, Analysis *source, int columnType);
	void				requestComputedColumnDestruction(	QString columnName);


	Q_INVOKABLE void	expandAnalysis();




public slots:
	void					setName(std::string name);
	void					setNameQ(QString name) { setName(name.toStdString()); }
	void					setHelpFile(QString helpFile);
	void					setTitleQ(QString title);
	void					setTitle(std::string title) { setTitleQ(tq(title)); }
	void					setHasVolatileNotes(bool hasVolatileNotes);
	void					refreshAvailableVariablesModels();
	void					emitDuplicationSignals();
	void					showDependenciesOnQMLForObject(QString uniqueName); //uniqueName is basically "name" in meta in results.

protected:
	void					bindOptionHandlers();

private:
	void					optionsChangedHandler(Option *option = nullptr);
	ComputedColumn *		requestComputedColumnCreationHandler(std::string columnName);
	void					requestColumnCreationHandler(std::string columnName, int colType)	{ requestColumnCreation(tq(columnName), this, colType); }
	void					requestComputedColumnDestructionHandler(std::string columnName);
	void					processResultsForDependenciesToBeShown();
	bool					processResultsForDependenciesToBeShownMetaTraverser(const Json::Value & array);
	bool					_editOptionsOfPlot(const Json::Value & results, const std::string & uniqueName, Json::Value & editOptions);
	bool					_setEditOptionsOfPlot(Json::Value & results, const std::string & uniqueName, const Json::Value & editOptions);
	void					storeUserDataEtc();
	void					fitOldUserDataEtc();

protected:
	Status					_status			= Initializing;
	bool					_refreshBlocked	= false;

	Options*				_options;

	///For backward compatibility: _optionsDotJASP = options from (old) JASP file.
	Json::Value				_optionsDotJASP = Json::nullValue,
							_results		= Json::nullValue,
							_meta			= Json::nullValue,
							_imgResults		= Json::nullValue,
							_userData		= Json::nullValue,
							_imgOptions		= Json::nullValue,
							_progress		= Json::nullValue,
							_oldUserData	= Json::nullValue,
							_oldMetaData	= Json::nullValue;

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
	bool					_useJaspResults		= false,
							_isDuplicate		= false,
							_wasUpgraded		= false,
							_hasVolatileNotes	= false,
							_tryToFixNotes		= false;
	Version					_version;
	int						_revision		= 0;

	Modules::AnalysisEntry*	_moduleData		= nullptr;
	Modules::DynamicModule* _dynamicModule	= nullptr;
	AnalysisForm*			_analysisForm	= nullptr;

	std::string				_codedReferenceToAnalysisEntry = "";
	QString					_helpFile;

	Modules::UpgradeMsgs	_msgs;
};

#endif // ANALYSIS_H
