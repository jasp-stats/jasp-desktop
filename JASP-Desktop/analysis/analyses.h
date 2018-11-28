//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef ANALYSES_H
#define ANALYSES_H

#include "analysis.h"
#include "appinfo.h"

#include <QString>
#include <QMap>
#include <QObject>

class Analyses : public QObject
{
	Q_OBJECT

	friend class EngineSync;
	friend class boost::iterator_core_access;

	typedef QMap<int, Analysis *> ById;
public:

				Analyses() {}

	Analysis*	createFromJaspFileEntry(Json::Value analysisData, DynamicModules * dynamicModules);
	Analysis*	create(const QString &module, const QString &name, size_t id, const Version &version, Json::Value *options = NULL, Analysis::Status status = Analysis::Empty);
	Analysis*	create(Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status = Analysis::Empty);

	Analysis*	create(const QString &module, const QString &name)	{ return create(module, name, _nextId++, AppInfo::version); }
	Analysis*	create(Modules::AnalysisEntry * analysisEntry)		{ return create(analysisEntry, _nextId++);					}

	Analysis*	get(size_t id) const	{ return id < _analyses.size() ? _analyses.at(id) : NULL; }
	void		clear();


	typedef std::vector<Analysis*>::iterator	iterator;

	iterator	begin()		{ return _analyses.begin(); }
	iterator	end()		{ return _analyses.end(); }

	int			count() const;

	Json::Value asJson() const;

signals:
	void analysisAdded(					Analysis *source);
	void analysisEditImage(				Analysis *source);
	void analysisSaveImage(				Analysis *source);
	void analysisToRefresh(				Analysis *source);
	void analysisImageSaved(			Analysis *source);
	void analysisInitialised(			Analysis *source);
	void analysisImageEdited(			Analysis *source);
	void analysisResultsChanged(		Analysis *source);
	void analysisOptionsChanged(		Analysis *source);

	ComputedColumn *	requestComputedColumnCreation(std::string columnName, Analysis *source);
	void				requestComputedColumnDestruction(std::string columnName);

private:
	void bindAnalysisHandler(Analysis* analysis);
	void storeAnalysis(Analysis* analysis, size_t id);

	void analysisOptionsChangedHandler(	Analysis *analysis)							{ analysisOptionsChanged(analysis); }
	void analysisImageSavedHandler(		Analysis *analysis)							{ analysisImageSaved(analysis); }
	void analysisImageEditedHandler(	Analysis *analysis)							{ analysisImageEdited(analysis); }
	void analysisResultsChangedHandler(	Analysis *analysis)							{ analysisResultsChanged(analysis); }
	void analysisToRefreshHandler(		Analysis *analysis);
	void analysisSaveImageHandler(		Analysis *analysis, Json::Value &options);
	void analysisEditImageHandler(		Analysis *analysis, Json::Value &options);

private:
	 std::vector<Analysis*> _analyses;

	size_t _nextId = 0;
};


#endif // ANALYSES_H
