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
#include "utilities/appdirs.h"
#include "processinfo.h"

#include <QFile>
#include <QTimer>

#include "utils.h"
#include "tempfiles.h"


using namespace std;


Analysis* Analyses::createFromJaspFileEntry(Json::Value analysisData, DynamicModules * dynamicModules )
{
	Analysis::Status status		= Analysis::parseStatus(analysisData["status"].asString());
	int id						= analysisData["id"].asInt();

	Analysis *analysis;

	if(analysisData.get("dynamicModule", Json::nullValue).isNull())
	{
		QString name				= QString::fromStdString(analysisData["name"].asString());
		QString module				= analysisData["module"].asString() != "" ? QString::fromStdString(analysisData["module"].asString()) : "Common";

		Json::Value &optionsJson	= analysisData["options"];
		Json::Value &versionJson	= analysisData["version"];

		Version version				= versionJson.isNull() ? AppInfo::version : Version(versionJson.asString());
		analysis					= create(module, name, id, version, &optionsJson, status);
	}
	else
		analysis = create(dynamicModules->retrieveCorrespondingAnalysisEntry(analysisData["dynamicModule"]), id, status);

	analysisAdded(analysis);

	analysis->setUserData(analysisData["userdata"]);
	analysis->setResults(analysisData["results"]);

	return analysis;
}

Analysis* Analyses::create(const QString &module, const QString &name, size_t id, const Version &version, Json::Value *options, Analysis::Status status)
{
	Analysis *analysis = AnalysisLoader::load(id, module.toStdString(), name.toStdString(), version, options);

	analysis->setStatus(status);
	storeAnalysis(analysis, id);
	bindAnalysisHandler(analysis);

	return analysis;
}

Analysis* Analyses::create(Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status)
{
	Analysis *analysis = new Analysis(id, analysisEntry);

	analysis->setStatus(status);
	storeAnalysis(analysis, id);
	bindAnalysisHandler(analysis);

	return analysis;
}

void Analyses::storeAnalysis(Analysis* analysis, size_t id)
{
	if (id >= _nextId)
		_nextId = id + 1;

	while (id >= _analyses.size())
		_analyses.push_back(NULL);

	_analyses[id] = analysis;
}

void Analyses::bindAnalysisHandler(Analysis* analysis)
{
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
	TempFiles::deleteAll(analysis->id());
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

Json::Value Analyses::asJson() const
{
	Json::Value analysesDataList = Json::arrayValue;

	for (Analysis *analysis : _analyses)
		if (analysis != NULL && analysis->isVisible())
			analysesDataList.append(analysis->asJSON());

	return analysesDataList;
}
