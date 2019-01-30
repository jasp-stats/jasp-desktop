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
#include "version.h"

#include "options/options.h"
#include "enginedefinitions.h"

#include <set>
#include <QObject>
#include "modules/dynamicmodules.h"

class ComputedColumn;
class Analyses;
class DataSet;
class AnalysisForm;

class Analysis : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString name READ nameQ NOTIFY nameChanged)
	
	typedef std::map<std::string, std::set<std::string>> optionColumns;

public:

	enum Status { Empty, Initing, Inited, Running, Complete, Aborting, Aborted, Error, SaveImg, EditImg, RewriteImgs, Exception, Initializing };
	void setStatus(Status status);

	Analysis(Analyses* analyses, size_t id, std::string module, std::string name, const Version &version, Json::Value *data);
	Analysis(Analyses* analyses, size_t id, Modules::AnalysisEntry * analysisEntry);

	virtual ~Analysis();

	void				resetOptionsFromJASPFile()			{ _optionsDotJASP.clear();	}
	Options				*options()					const	{ return _options;			}
	const Json::Value&	optionsFromJASPFile()		const	{ return _optionsDotJASP;	}

signals:
	void				nameChanged();
	void				sendRScript(			Analysis * analysis, QString script, QString controlName);
	void				optionsChanged(			Analysis * analysis);
	void				saveImageSignal(		Analysis * analysis);
	void				editImageSignal(		Analysis * analysis);
	void				toRefreshSignal(		Analysis * analysis);
	void				imageSavedSignal(		Analysis * analysis);
	void				imageEditedSignal(		Analysis * analysis);
	void				rewriteImagesSignal(	Analysis * analysis);
	void				resultsChangedSignal(	Analysis * analysis);

	ComputedColumn *	requestComputedColumnCreation(		QString columnName, Analysis * analysis);
	void				requestComputedColumnDestruction(	QString columnName);

public:
	bool isWaitingForModule()	{ return _moduleData == nullptr ? false : !_moduleData->dynamicModule()->readyForUse(); }
	bool isDynamicModule()		{ return _moduleData == nullptr ? false : _moduleData->dynamicModule() != nullptr; }

	void setResults(	const Json::Value & results, int progress = -1);
	void imageSaved(	const Json::Value & results);
	void saveImage(		const Json::Value & options);
	void editImage(		const Json::Value & options);
	void imageEdited(	const Json::Value & results);
	void rewriteImages();
	void imagesRewritten();

	void setRFile(const std::string &file)				{ _rfile = file;							}
	void setVisible(bool visible)						{ _visible = visible;						}
	void setUserData(Json::Value userData)				{ _userData = userData;						}
	void setRefreshBlocked(bool block)					{ _refreshBlocked = block;					}
	void setUsesJaspResults(bool usesJaspResults)		{ _useJaspResults = usesJaspResults;		}
	
	//getters
	const	Json::Value		&	results()			const	{ return _results;						}
	const	Json::Value		&	userData()			const	{ return _userData;						}
	const	std::string		&	name()				const	{ return _name;							}
	const	QString				nameQ()				const	{ return QString::fromStdString(_name);	}
	const	Version			&	version()			const	{ return _version;						}
	const	std::string		&	title()				const	{ return _title;						}
	const	std::string		&	rfile()				const	{ return _rfile;						}
	const	std::string		&	module()			const	{ return _module;						}
			size_t				id()				const	{ return _id;							}
			bool				usesJaspResults()	const	{ return _useJaspResults;				}
			Status				status()			const	{ return _status;						}
			int					revision()			const	{ return _revision;						}
			bool				isVisible()			const	{ return _visible;						}
			bool				isRefreshBlocked()	const	{ return _refreshBlocked;				}
	const	Json::Value		&	getSaveImgOptions()	const	{ return _saveImgOptions;				}
	const	Json::Value		&	getImgResults()		const	{ return _imgResults;					}
			DataSet			*	getDataSet()		const;
	Modules::DynamicModule	*	dynamicModule()		const	{ return _moduleData == nullptr ? nullptr : _moduleData->dynamicModule(); }

			void		refresh();
	virtual void		abort();

			Json::Value asJSON()		const;
			Json::Value createAnalysisRequestJson(int ppi, std::string imageBackground);

	static	Status		parseStatus(std::string name);

	bool isEmpty()			const { return status() == Empty;		}
	bool isAborted()		const { return status() == Aborted;		}
	bool isSaveImg()		const { return status() == SaveImg;		}
	bool isRewriteImgs()	const { return status() == RewriteImgs;	}
	bool isEditImg()		const { return status() == EditImg;		}
	bool isInited()			const { return status() == Inited;		}
	bool isFinished()		const { return status() == Complete || status() == Error || status() == Exception; }
	
	void initialized(AnalysisForm* form, bool isNewAnalysis);

	performType				desiredPerformTypeFromAnalysisStatus() const;
	std::string				qmlFormPath() const;

	std::set<std::string>	usedVariables()													{ return _options->usedVariables();					}
	std::set<std::string>	columnsCreated()												{ return _options->columnsCreated();				}
	void					removeUsedVariable(std::string var)								{ _options->removeUsedVariable(var);				}
	void					replaceVariableName(std::string oldName, std::string newName)	{ _options->replaceVariableName(oldName, newName);	}	
	void					runScriptRequestDone(const QString& result, const QString& controlName);

public slots:
	void					setName(std::string name);
	void					setNameQ(QString name) { setName(name.toStdString()); }


	
protected:
	int						callback(Json::Value results);
	void					bindOptionHandlers();

private:
	void					optionsChangedHandler(Option *option);
	ComputedColumn *		requestComputedColumnCreationHandler(std::string columnName)	{ return requestComputedColumnCreation(QString::fromStdString(columnName), this); }
	void					requestComputedColumnDestructionHandler(std::string columnName) { requestComputedColumnDestruction(QString::fromStdString(columnName)); }

protected:
	Status					_status			= Initializing;
	bool					_visible		= true,
							_refreshBlocked	= false;

	Options*				_options;

	///For backward compatibility: options coming form old JASP file.
	Json::Value				_optionsDotJASP = Json::nullValue, ///
							_results		= Json::nullValue,
							_imgResults		= Json::nullValue,
							_userData		= Json::nullValue,
							_saveImgOptions	= Json::nullValue;
	int						_progress		= -1;

private:
	size_t					_id;
	std::string				_module			= "dynamic",
							_name,
							_title,
							_rfile;
	bool					_useJaspResults = false;
	Version					_version;
	int						_revision		= 0;

	Modules::AnalysisEntry*	_moduleData		= nullptr;
	Analyses*				_analyses		= nullptr;
	AnalysisForm*			_analysisForm	= nullptr;	
};

#endif // ANALYSIS_H
