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
#include "modules/dynamicmodules.h"

class ComputedColumn;

class Analysis
{
	typedef std::map<std::string, std::set<std::string>> optionColumns;

public:

	enum Status { Empty, Initing, Inited, InitedAndWaiting, Running, Complete, Aborting, Aborted, Error, SaveImg, EditImg, Exception };

	Analysis(int id, std::string module, std::string name, std::string title, Json::Value &requiresInit, Json::Value &dataKey, Json::Value &stateKey, Json::Value &resultsMeta, Json::Value optionsJson, const Version &version, Json::Value *data, bool isAutorun = true, bool usedata = true, bool fromQML = false, bool useJaspResults = false);
	Analysis(int id, Modules::AnalysisEntry * analysisEntry);

	virtual ~Analysis();

	Options *options() const { return _options; }

	boost::signals2::signal<void (Analysis *source)>						optionsChanged;
	boost::signals2::signal<void (Analysis *source)>						toRefresh;
	boost::signals2::signal<void (Analysis *source, Json::Value &options)>	saveImage;
	boost::signals2::signal<void (Analysis *source)>						imageSaved;
	boost::signals2::signal<void (Analysis *source, Json::Value &options)>	editImage;
	boost::signals2::signal<void (Analysis *source)>						imageEdited;
	boost::signals2::signal<void (Analysis *source)>						resultsChanged;

	boost::signals2::signal<void				(std::string columnName)>														requestComputedColumnDestruction;
	boost::signals2::signal<ComputedColumn *	(std::string columnName, Analysis *source), return_not_NULL<ComputedColumn *>>	requestComputedColumnCreation;

	bool isWaitingForModule() { return _moduleData == NULL ? false : !_moduleData->dynamicModule()->readyForUse(); }


	void setResults(Json::Value results, int progress = -1);
	void setImageResults(Json::Value results);
	void setImageEdited(Json::Value results);
	void setStatus(Status status);

	void setRFile(const std::string &file)				{ _rfile = file;							}
	void setVisible(bool visible)						{ _visible = visible;						}
	void setUserData(Json::Value userData)				{ _userData = userData;						}
	void setRefreshBlocked(bool block)					{ _refreshBlocked = block;					}
	void setSaveImgOptions(Json::Value &options)		{ _saveImgOptions = options;				}
	void setUsesJaspResults(bool usesJaspResults)		{ _useJaspResults = usesJaspResults;		}
	
	//getters
	const	Json::Value &results()				const	{ return _results;				}
	const	Json::Value &userData()				const	{ return _userData;				}
	const	Json::Value &requiresInit()			const	{ return _requiresInit;			}
	const	Json::Value &dataKey()				const	{ return _dataKey;				}
	const	Json::Value &stateKey()				const	{ return _stateKey;				}
	const	Json::Value &resultsMeta()			const	{ return _resultsMeta;			}
	const	std::string &name()					const	{ return _name;					}
	const	std::string &title()				const	{ return _title;				}
	const	std::string &rfile()				const	{ return _rfile;				}
	const	std::string &module()				const	{ return _module;				}
			int			id()					const	{ return _id;					}
			bool		isAutorun()				const	{ return _autorun;				}
			bool		useData()				const	{ return _usedata;				}
			bool		fromQML()				const	{ return _fromQML;				}
			bool		usesJaspResults()		const	{ return _useJaspResults;		}
			Status		status()				const	{ return _status;				}
			int			revision()				const	{ return _revision;				}
			bool		isVisible()				const	{ return _visible;				}
			bool		isRefreshBlocked()		const	{ return _refreshBlocked;		}
	const	Json::Value	&getSaveImgOptions()	const	{ return _saveImgOptions;		}
	const	Json::Value	&getImgResults()		const	{ return _imgResults;			}

			void		refresh();
	virtual void		abort();
			void		scheduleRun();

			Json::Value asJSON()		const;
			Json::Value createAnalysisRequestJson(int ppi);

	static	Status		parseStatus(std::string name);

			bool		isEmpty()		const { return status() == Empty;   }
			bool		isAborted()		const { return status() == Aborted; }
			bool		isSaveImg()		const { return status() == SaveImg; }
			bool		isEditImg()		const { return status() == EditImg; }
			bool		isInited()		const { return status() == Inited;  }

			performType				desiredPerformTypeFromAnalysisStatus() const;
			std::string				qmlFormPath() const;

			std::set<std::string>	usedVariables()													{ return _options->usedVariables();					}
			std::set<std::string>	columnsCreated()												{ return _options->columnsCreated();				}
			void					removeUsedVariable(std::string var)								{ _options->removeUsedVariable(var);				}
			void					replaceVariableName(std::string oldName, std::string newName)	{ _options->replaceVariableName(oldName, newName);	}

protected:
	int						callback(Json::Value results);
	void					bindOptionHandlers();

private:
	void					optionsChangedHandler(Option *option);
	ComputedColumn *		requestComputedColumnCreationHandler(std::string columnName)	{ return requestComputedColumnCreation(columnName, this); }
	void					requestComputedColumnDestructionHandler(std::string columnName) { requestComputedColumnDestruction(columnName); }

protected:
	Status					_status			= Empty;
	bool					_visible		= true,
							_refreshBlocked	= false;

	Options*				_options;
	Json::Value				_results		= Json::nullValue,
							_imgResults		= Json::nullValue,
							_userData		= Json::nullValue,
							_saveImgOptions	= Json::nullValue;
	int						_progress		= -1;

private:
	int						_id;
	std::string				_module			= "dynamic",
							_name,
							_title,
							_rfile;
	Json::Value				_requiresInit	= false,
							_dataKey		= Json::nullValue,
							_stateKey		= Json::nullValue,
							_resultsMeta	= Json::nullValue;
	bool					_autorun		= true,
							_usedata		= true,
							_fromQML		= false,
							_useJaspResults = false;
	Version					_version;
	int						_revision		= 0;

	Modules::AnalysisEntry	*_moduleData	= NULL;
};

#endif // ANALYSIS_H
