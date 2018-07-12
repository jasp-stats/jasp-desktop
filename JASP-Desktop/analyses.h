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

#include <QString>
#include <QMap>
#include <QObject>

class Analyses : public QObject
{
	Q_OBJECT

	friend class EngineSync;
	friend class boost::iterator_core_access;

	typedef QMap<int, Analysis *>				ById;
public:

				Analyses() : _nextId(0) {}

	Analysis*	create(const QString &module, const QString &name, int id, const Version &version, Json::Value *options = NULL, Analysis::Status status = Analysis::Empty);
	Analysis*	create(const QString &module, const QString &name);

	Analysis*	get(int id) const	{ return id < _analyses.size() ? _analyses.at(id) : NULL; }
	void		clear();


	typedef std::vector<Analysis*>::iterator	iterator;

	iterator	begin()		{ return _analyses.begin(); }
	iterator	end()		{ return _analyses.end(); }

	int			count() const;

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
	void analysisOptionsChangedHandler(	Analysis *analysis)							{ analysisOptionsChanged(analysis); }
	void analysisImageSavedHandler(		Analysis *analysis)							{ analysisImageSaved(analysis); }
	void analysisImageEditedHandler(	Analysis *analysis)							{ analysisImageEdited(analysis); }
	void analysisResultsChangedHandler(	Analysis *analysis)							{ analysisResultsChanged(analysis); }
	void analysisToRefreshHandler(		Analysis *analysis);
	void analysisSaveImageHandler(		Analysis *analysis, Json::Value &options);
	void analysisEditImageHandler(		Analysis *analysis, Json::Value &options);

	 std::vector<Analysis*> _analyses;

	int _nextId;
};


#endif // ANALYSES_H
