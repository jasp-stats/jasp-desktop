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
	size_t id					= analysisData["id"].asUInt();

	if(_nextId <= id) _nextId = id + 1;

	Analysis *analysis;

	if(analysisData.get("dynamicModule", Json::nullValue).isNull())
	{
		QString name				= QString::fromStdString(analysisData["name"].asString());
		QString module				= analysisData["module"].asString() != "" ? QString::fromStdString(analysisData["module"].asString()) : "Common";

		bool fromQML				= analysisData.get("fromQML", false).asBool();
		if (fromQML)
			name = QString::fromLatin1("QML") + name; // temporary hack: remove this when everything is build with QML
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
	if(_analysisMap.count(id) > 0)
		throw std::runtime_error("Analysis with id="+std::to_string(id)+" already registered!");

	if (id >= _nextId)
		_nextId = id + 1;

	int newRowNum = int(count());

	beginInsertRows(QModelIndex(), newRowNum, newRowNum);
	_analysisMap[id] = analysis;
	_orderedIds.push_back(id);
	endInsertRows();

	emit countChanged();
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
	beginResetModel();
	for (auto idAnalysis : _analysisMap)
		delete idAnalysis.second;

	_analysisMap.clear();
	_orderedIds.clear();

	_nextId = 0;
	endResetModel();
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

	for(auto idAnalysis : _analysisMap)
			analysesDataList.append(idAnalysis.second->asJSON());

	return analysesDataList;
}


void Analyses::removeAnalysis(Analysis *analysis)
{
	size_t id = analysis->id();

	long indexAnalysis = -1;
	for(size_t i=_orderedIds.size(); i>0; i--)
		if(_orderedIds[i-1] == id)
		{
			indexAnalysis = long(i) - 1;
			break;
		}

	beginRemoveRows(QModelIndex(), indexAnalysis, indexAnalysis);
	analysis->abort();
	analysis->setVisible(false);
	_analysisMap.erase(id);
	_orderedIds.erase(_orderedIds.begin() + indexAnalysis);
	endRemoveRows();

	emit countChanged();
}

void Analyses::refreshAllAnalyses()
{
	for(auto idAnalysis : _analysisMap)
		idAnalysis.second->refresh();
}


void Analyses::refreshAnalysesUsingColumn(QString col)
{
	std::vector<std::string> changedColumns, missingColumns, oldNames;
	std::map<std::string, std::string> changeNameColumns;
	changedColumns.push_back(col.toStdString());

	refreshAnalysesUsingColumns(changedColumns, missingColumns, changeNameColumns, oldNames);
}

void Analyses::removeAnalysisById(size_t id)
{
	Analysis *analysis = get(id);
	removeAnalysis(analysis);
}

void Analyses::setAnalysesUserData(Json::Value userData)
{
	for (Json::Value &userDataObj  : userData)
	{
		Analysis *analysis				= get(size_t(userDataObj["id"].asInt()));
		Json::Value &analysisUserData	= userDataObj["userdata"];

		analysis->setUserData(analysisUserData);
	}
}


void Analyses::refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns,	 std::vector<std::string> &missingColumns,	 std::map<std::string, std::string> &changeNameColumns,	 std::vector<std::string> &oldColumnNames)
{
	std::set<Analysis *> analysesToRefresh;

	for (auto idAnalysis : _analysisMap)
	{
		Analysis * analysis = idAnalysis.second;

		std::set<std::string> variables = analysis->usedVariables();

		if (!variables.empty())
		{
			std::vector<std::string> interChangecol, interChangename, interMissingcol;

			std::set_intersection(variables.begin(), variables.end(), changedColumns.begin(), changedColumns.end(), std::back_inserter(interChangecol));
			std::set_intersection(variables.begin(), variables.end(), oldColumnNames.begin(), oldColumnNames.end(), std::back_inserter(interChangename));
			std::set_intersection(variables.begin(), variables.end(), missingColumns.begin(), missingColumns.end(), std::back_inserter(interMissingcol));

			bool	aNameChanged	= interChangename.size() > 0,
					aColumnRemoved	= interMissingcol.size() > 0,
					aColumnChanged	= interChangecol.size() > 0;

			if(aNameChanged || aColumnRemoved)
				analysis->setRefreshBlocked(true);

			if (aColumnRemoved)
				for (std::string & varname : interMissingcol)
					analysis->removeUsedVariable(varname);

			if (aNameChanged)
				for (std::string & varname : interChangename)
					analysis->replaceVariableName(varname, changeNameColumns[varname]);

			if (aNameChanged || aColumnRemoved || aColumnChanged)
				analysesToRefresh.insert(analysis);
		}
	}

	for (Analysis *analysis : analysesToRefresh)
	{
		analysis->setRefreshBlocked(false);
		analysis->refresh();
	}
}

/* the information about the used modules must be made available to RibbonModel somehow
void MainWindow::checkUsedModules()
{
	QStringList usedModules;
	for (Analyses::iterator itr = _analyses->begin(); itr != _analyses->end(); itr++)
	{
		Analysis *analysis = *itr;
		if (analysis != nullptr && analysis->isVisible())
		{
			QString moduleName = QString::fromStdString(analysis->module());
			if (!usedModules.contains(moduleName))
				usedModules.append(moduleName);
		}
	}

	std::cout << "Used modules not being added to plus menu" << std::endl;
	//ui->tabBar->setModulePlusMenu(usedModules);
}
*/

void Analyses::applyToSome(std::function<bool(Analysis *analysis)> applyThis)
{
	for(size_t id : _orderedIds)
		if(!applyThis(_analysisMap[id]))
			return;
}

void Analyses::applyToAll(std::function<void(Analysis *analysis)> applyThis)
{
	for(size_t id : _orderedIds)
		applyThis(_analysisMap[id]);
}

QVariant Analyses::data(const QModelIndex &index, int role)	const
{
	if(index.row() < 0 || index.row() > rowCount())
		return QVariant();

	size_t	row = size_t(index.row()),
			id  = _orderedIds[row];

	Analysis * analysis = _analysisMap.at(id);

	switch(role)
	{
	case formPathRole:		return QString::fromStdString(analysis->qmlFormPath());
	case Qt::DisplayRole:
	case titleRole:			return QString::fromStdString(analysis->title());
	case nameRole:			return QString::fromStdString(analysis->name());
	default:				return QVariant();
	}
}

QHash<int, QByteArray>	Analyses::roleNames() const
{
	static const QHash<int, QByteArray> roles = {
		{ formPathRole, "formPath"		},
		{ titleRole,	"displayText"			},
		{ nameRole,		"name"	} };

	return roles;
}

void Analyses::analysisClickedHandler(QString analysisFunction, QString module)
{
	Analysis* analysis = create(module, analysisFunction);
	analysisAdded(analysis);

	emit analysisAdded(analysis);
}

void Analyses::setCurrentAnalysisIndex(int currentAnalysisIndex)
{
	if (_currentAnalysisIndex == currentAnalysisIndex)
		return;

	_currentAnalysisIndex = currentAnalysisIndex;
	emit currentAnalysisIndexChanged(_currentAnalysisIndex);
}
