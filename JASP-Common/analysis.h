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

class Analysis
{
public:

	enum Status { Empty, Initing, Inited, InitedAndWaiting, Running, Complete, Aborting, Aborted, Error, SaveImg, EditImg, Exception };

	Analysis(int id, std::string module, std::string name, std::string title, Json::Value &requiresInit, Json::Value &dataKey, Json::Value &stateKey, Json::Value &resultsMeta, Options *options, const Version &version, bool isAutorun = true, bool usedata = true, bool useJaspResults = false);
	virtual ~Analysis();

	Options *options() const;

	boost::signals2::signal<void (Analysis *source)>						optionsChanged;
	boost::signals2::signal<void (Analysis *source)>						toRefresh;
	boost::signals2::signal<void (Analysis *source, Json::Value &options)>	saveImage;
	boost::signals2::signal<void (Analysis *source)>						imageSaved;
	boost::signals2::signal<void (Analysis *source, Json::Value &options)>	editImage;
	boost::signals2::signal<void (Analysis *source)>						imageEdited;
	boost::signals2::signal<void (Analysis *source)>						resultsChanged;

	void setResults(Json::Value results, int progress = -1);
	void setImageResults(Json::Value results);
	void setImageEdited(Json::Value results);
	void setUserData(Json::Value userData);

	//getters
	const	Json::Value &results()			const;
	const	Json::Value &userData()			const;
			Json::Value asJSON()			const;
	const	Json::Value &requiresInit()		const;
	const	Json::Value &dataKey()			const;
	const	Json::Value &stateKey()			const;
	const	Json::Value &resultsMeta()		const;
	const	std::string &name()				const;
	const	std::string &title()			const;
	const	std::string &module()			const;
			int			id()				const;
			bool		isAutorun()			const;
			bool		useData()			const;
			bool		usesJaspResults()	const { return _jaspResultsAnalysis; }


			void refresh();
	virtual void abort();
			void scheduleRun();

	Status	status() const;
	void	setStatus(Status status);

	bool isVisible();
	void setVisible(bool visible);

	bool isRefreshBlocked();
	void setRefreshBlocked(bool block);

	int revision();

	void		setSaveImgOptions(Json::Value &options);
	Json::Value getSaveImgOptions();
	Json::Value getImgResults();

	static Status parseStatus(std::string name);


protected:

	Status _status;
	bool _visible			= true;
	bool _refreshBlocked	= false;

	Options* _options;
	Json::Value	_results		= Json::nullValue,
				_imgResults		= Json::nullValue,
				_userData		= Json::nullValue,
				_saveImgOptions	= Json::nullValue;
	int _progress;

	int callback(Json::Value results);

private:
	int _id;
	std::string	_module,
				_name,
				_title;
	Json::Value _requiresInit,
				_dataKey,
				_stateKey,
				_resultsMeta;
	bool		_autorun,
				_usedata,
				_jaspResultsAnalysis;
	Version		_version;
	int			_revision = 0;

	void optionsChangedHandler(Option *option);


};

#endif // ANALYSIS_H
