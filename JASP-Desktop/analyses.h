//
// Copyright (C) 2013-2017 University of Amsterdam
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

	typedef QMap<int, Analysis *> ById;

public:
	Analyses();

	Analysis *create(const QString &name, Json::Value *options = NULL, Analysis::Status status = Analysis::Empty);
	Analysis *create(const QString &name, int id, Json::Value *options = NULL, Analysis::Status status = Analysis::Empty);
	Analysis *get(int id) const;
	void clear();

	typedef QList<Analysis*>::iterator iterator;
	iterator begin();
	iterator end();

	int count() const;

signals:
	void analysisInitialised(Analysis *source);
	void analysisOptionsChanged(Analysis *source);
	void analysisToRefresh(Analysis *source);
	void analysisSaveImage(Analysis *source);
	void analysisImageSaved(Analysis *source);
	void analysisResultsChanged(Analysis *source);
	void analysisUserDataLoaded(Analysis *source);
	void analysisAdded(Analysis *source);

private slots:
	void flushDefaultsToDisk();

private:

	typedef struct {
		QString analysisName;
		Options *options;
		bool needsSync;
	} Defaults;

	void assignDefaults(Analysis *analysis);

	void analysisOptionsChangedHandler(Analysis *analysis);
	void analysisToRefreshHandler(Analysis *analysis);
	void analysisSaveImageHandler(Analysis *analysis, Json::Value &options);
	void analysisImageSavedHandler(Analysis *analysis);
	void analysisResultsChangedHandler(Analysis *analysis);
	void analysisUserDataLoadedHandler(Analysis *analysis);

	QList<Analysis*> _analyses;

	int _nextId;

	QMap<QString, Defaults> _defaults;



};


#endif // ANALYSES_H
