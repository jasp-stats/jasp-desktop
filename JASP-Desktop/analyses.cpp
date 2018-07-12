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

#include "analyses.h"

#include "analysisloader.h"
#include "boost/foreach.hpp"
#include "appdirs.h"
#include "processinfo.h"

#include <QFile>
#include <QTimer>

#include "utils.h"
#include "tempfiles.h"
#include "appinfo.h"

using namespace std;

Analysis* Analyses::create(const QString &module, const QString &name)
{
	return create(module, name, _nextId++, AppInfo::version);
}

Analysis* Analyses::create(const QString &module, const QString &name, int id, const Version &version, Json::Value *options, Analysis::Status status)
{
	if (id >= _nextId)
		_nextId = id + 1;

	Analysis *analysis = AnalysisLoader::load(id, module.toStdString(), name.toStdString(), version, options);
	analysis->setStatus(status);

	while (id >= _analyses.size())
		_analyses.push_back(NULL);

	_analyses[id] = analysis;

	analysis->toRefresh.connect(						boost::bind( &Analyses::analysisToRefreshHandler,			this, _1	 ));
	analysis->saveImage.connect(						boost::bind( &Analyses::analysisSaveImageHandler,			this, _1, _2 ));
	analysis->editImage.connect(						boost::bind( &Analyses::analysisEditImageHandler,			this, _1, _2 ));
	analysis->imageSaved.connect(						boost::bind( &Analyses::analysisImageSavedHandler,			this, _1	 ));
	analysis->imageEdited.connect(						boost::bind( &Analyses::analysisImageEditedHandler,			this, _1	 ));
	analysis->optionsChanged.connect(					boost::bind( &Analyses::analysisOptionsChangedHandler,		this, _1	 ));
	analysis->resultsChanged.connect(					boost::bind( &Analyses::analysisResultsChangedHandler,		this, _1	 ));
	analysis->requestComputedColumnCreation.connect(	boost::bind( &Analyses::requestComputedColumnCreation,		this, _1, _2 ));
	analysis->requestComputedColumnDestruction.connect(	boost::bind( &Analyses::requestComputedColumnDestruction,	this, _1	 ));

//	Send the analysesAdded signal afterwards: the analysis may need extra settings after creation
//	analysisAdded(analysis);

	return analysis;
}

void Analyses::clear()
{
	for (Analysis *analysis : _analyses)
		delete analysis;

	_analyses.clear();
}


int Analyses::count() const
{
	int c = 0;

	for (Analysis *analysis : _analyses)
		if (analysis != NULL)
			c++;

	return c;
}

void Analyses::analysisToRefreshHandler(Analysis *analysis)
{
	analysis->setStatus(Analysis::Empty);
	tempfiles_deleteAll(analysis->id());
	analysisToRefresh(analysis);
}
void Analyses::analysisSaveImageHandler(Analysis *analysis, Json::Value &options)
{
	analysis->setStatus(Analysis::SaveImg);
	analysis->setSaveImgOptions(options);
	analysisSaveImage(analysis);
}

void Analyses::analysisEditImageHandler(Analysis *analysis, Json::Value &options)
{
    analysis->setStatus(Analysis::EditImg);
    analysis->setSaveImgOptions(options); // options from saveImage are fine
    analysisEditImage(analysis);
}

