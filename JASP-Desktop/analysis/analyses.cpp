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

#include "boost/foreach.hpp"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "processinfo.h"
#include "modules/ribbonmodel.h"

#include <QFile>
#include <QTimer>


#include "utils.h"
#include "tempfiles.h"
#include "log.h"


using namespace std;


void Analyses::_makeBackwardCompatible(RibbonModel* ribbonModel, Version &version, Json::Value &analysisData)
{
	Version			V0_9_3('0', '9', '3', '0');

	if (version <= V0_9_3)
	{
		std::string module = analysisData["module"].asString();
		if (module.empty())
			module = "Common";

		// An old JASP file may still have references to the old Common module.
		if (module == "Common")
		{
			QString	name = QString::fromStdString(analysisData["name"].asString());
			module = ribbonModel->getModuleNameFromAnalysisName(name).toStdString();
		}
		else if (module == "MetaAnalysis")
			module = "Meta Analysis";
		else if (module == "SummaryStats")
			module = "Summary Statistics";

		analysisData["module"] = module;
	}
}


Analysis* Analyses::createFromJaspFileEntry(Json::Value analysisData, RibbonModel* ribbonModel)
{
	Analysis::Status status		= Analysis::parseStatus(analysisData["status"].asString());
	size_t id					= analysisData["id"].asUInt();

	if(_nextId <= id) _nextId = id + 1;

	Analysis *analysis;

	if(analysisData.get("dynamicModule", Json::nullValue).isNull())
	{
		Json::Value	&	versionJson		= analysisData["version"];
		Version			version			= versionJson.isNull() ? AppInfo::version : Version(versionJson.asString());
		_makeBackwardCompatible(ribbonModel, version, analysisData);


		QString			name				= QString::fromStdString(analysisData["name"].asString()),
						module				= analysisData["module"].asString() != "" ? QString::fromStdString(analysisData["module"].asString()) : "Common",
						title				= QString::fromStdString(analysisData.get("title", "").asString());

		Json::Value &	optionsJson	= analysisData["options"];

		auto		*	analysisEntry	= ribbonModel->getAnalysis(module.toStdString(), name.toStdString());

		if(title == "")
			title = analysisEntry ? QString::fromStdString(analysisEntry->title()) : name;
		
						analysis		= create(module, name, title, id, version, &optionsJson, status, false);
	}
	else
	{
		std::string title			= analysisData.get("title", "").asString();
		auto *	analysisEntry		= _dynamicModules->retrieveCorrespondingAnalysisEntry(analysisData["dynamicModule"]);
				analysis			= create(analysisEntry, id, status, false, title);
		auto *	dynMod				= analysisEntry->dynamicModule();

		if(!dynMod->loaded())
			dynMod->setLoadingNeeded();

	}

	analysis->setUserData(analysisData["userdata"]);
	analysis->setResults(analysisData["results"]);

	return analysis;
}

Analysis* Analyses::create(const QString &module, const QString &name, const QString &title, size_t id, const Version &version, Json::Value *options, Analysis::Status status, bool notifyAll)
{
	Analysis *analysis = new Analysis(this, id, module.toStdString(), name.toStdString(), title.toStdString(), version, options);
	analysis->setStatus(status);
	storeAnalysis(analysis, id, notifyAll);
	bindAnalysisHandler(analysis);

	return analysis;
}

Analysis* Analyses::create(Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status, bool notifyAll, std::string title)
{
	Analysis *analysis = new Analysis(this, id, analysisEntry, title);

	analysis->setStatus(status);
	analysis->setResults(analysisEntry->getDefaultResults());
	storeAnalysis(analysis, id, notifyAll);
	bindAnalysisHandler(analysis);

	return analysis;
}

void Analyses::storeAnalysis(Analysis* analysis, size_t id, bool notifyAll)
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

	if(notifyAll)
	{
		emit analysisAdded(analysis);
		setCurrentAnalysisIndex(_orderedIds.size() - 1);
		showAnalysisInResults(id);
	}
}

void Analyses::bindAnalysisHandler(Analysis* analysis)
{
	connect(analysis, &Analysis::optionsChanged,					this, &Analyses::analysisOptionsChanged				);
	connect(analysis, &Analysis::sendRScript,						this, &Analyses::sendRScriptHandler					);
	connect(analysis, &Analysis::toRefreshSignal,					this, &Analyses::analysisToRefresh					);
	connect(analysis, &Analysis::saveImageSignal,					this, &Analyses::analysisSaveImage					);
	connect(analysis, &Analysis::editImageSignal,					this, &Analyses::analysisEditImage					);
	connect(analysis, &Analysis::imageSavedSignal,					this, &Analyses::analysisImageSaved					);
	connect(analysis, &Analysis::rewriteImagesSignal,				this, &Analyses::analysisRewriteImages				);
	connect(analysis, &Analysis::imageEditedSignal,					this, &Analyses::analysisImageEdited				);
	connect(analysis, &Analysis::requestColumnCreation,				this, &Analyses::requestColumnCreation				);
	connect(analysis, &Analysis::resultsChangedSignal,				this, &Analyses::analysisResultsChanged				);
	connect(analysis, &Analysis::requestComputedColumnCreation,		this, &Analyses::requestComputedColumnCreation		);
	connect(analysis, &Analysis::requestComputedColumnDestruction,	this, &Analyses::requestComputedColumnDestruction	);

	
	if (Settings::value(Settings::DEVELOPER_MODE).toBool())
	{
		QString filePath = QString::fromStdString(analysis->qmlFormPath());
		
		if (filePath.startsWith("file:"))
			filePath.remove(0,5);
		if (!_QMLFileWatcher.files().contains(filePath))
		{
			if (!_QMLFileWatcher.addPath(filePath))
				Log::log()  << "Could not watch: " << filePath.toStdString() << std::endl;
		}
		connect(&_QMLFileWatcher, &QFileSystemWatcher::fileChanged, [=] () { this->_analysisQMLFileChanged(analysis); });
	}
}

void Analyses::clear()
{
	beginResetModel();
	for (auto idAnalysis : _analysisMap)
	{
		Analysis* analysis = idAnalysis.second;
		emit analysisRemoved(analysis);
		delete analysis;
	}

	_analysisMap.clear();
	_orderedIds.clear();

	_nextId = 0;
	endResetModel();
	emit countChanged();
}

void Analyses::reload(Analysis *analysis)
{
	size_t i = 0;
	for (; i < _orderedIds.size(); i++)
		if (_analysisMap[_orderedIds[i]] == analysis) break;

	if (i < _orderedIds.size())
	{
		int ind = int(i);
		// Force the loader to load again the QML file
		beginRemoveRows(QModelIndex(), ind, ind);
		endRemoveRows();
		beginInsertRows(QModelIndex(), ind, ind);
		endInsertRows();
	}
	else
		Log::log() << "Analysis " << analysis->title() << " not found!" << std::endl << std::flush;
}

void Analyses::_analysisQMLFileChanged(Analysis *analysis)
{
	emit emptyQMLCache();
	reload(analysis);
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

	int indexAnalysis = -1;
	for(size_t i=_orderedIds.size(); i>0; i--)
		if(_orderedIds[i-1] == id)
		{
			indexAnalysis = int(i) - 1;
			break;
		}

	beginRemoveRows(QModelIndex(), indexAnalysis, indexAnalysis);
	analysis->abort();
	analysis->setVisible(false);
	_analysisMap.erase(id);
	_orderedIds.erase(_orderedIds.begin() + indexAnalysis);
	endRemoveRows();

	emit countChanged();
	emit analysisRemoved(analysis);

	delete analysis;
}


void Analyses::removeAnalysesOfDynamicModule(Modules::DynamicModule * module)
{
	std::set<int> removeIds;
	for(auto & keyval : _analysisMap)
		if(keyval.second->dynamicModule() == module)
			removeIds.insert(keyval.first);

	for(const int & id : removeIds)
		removeAnalysisById(size_t(id));
}

void Analyses::refreshAnalysesOfDynamicModule(Modules::DynamicModule * module)
{
	for(auto & keyval : _analysisMap)
		if(keyval.second->dynamicModule() == module)
			keyval.second->refresh();
}

void Analyses::rescanAnalysisEntriesOfDynamicModule(Modules::DynamicModule * module)
{

	std::set<int> removeIds;
	for(auto & keyval : _analysisMap)
		if(keyval.second->dynamicModule() == module  && !keyval.second->checkAnalysisEntry()) // Check if the analysisEntry this analysis is based still exists
			removeIds.insert(keyval.first);

	for(const int & id : removeIds)
		removeAnalysisById(size_t(id));
}

void Analyses::refreshAllAnalyses()
{
	for(auto idAnalysis : _analysisMap)
		idAnalysis.second->refresh();
}

void Analyses::refreshAllPlots(std::set<Analysis*> exceptThese)
{
	for(auto idAnalysis : _analysisMap)
		if(exceptThese.count(idAnalysis.second) == 0)
			idAnalysis.second->rewriteImages();
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

		if(analysis != nullptr)
		{
			Json::Value &analysisUserData	= userDataObj["userdata"];
			analysis->setUserData(analysisUserData);
		}
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
	case analysisRole:		return QVariant::fromValue(analysis);
	case idRole:			return int(analysis->id());
	default:				return QVariant();
	}
}

QHash<int, QByteArray>	Analyses::roleNames() const
{
	static const QHash<int, QByteArray> roles = {
		{ formPathRole,		"formPath"		},
		{ titleRole,		"displayText"	},
		{ analysisRole,		"analysis"		},
		{ nameRole,			"name"			},
		{ idRole,			"analysisID"	} };

	return roles;
}

void Analyses::analysisClickedHandler(QString analysisName, QString analysisTitle, QString module)
{
	Modules::DynamicModule * dynamicModule = _dynamicModules->dynamicModule(module.toStdString());

	if(dynamicModule != nullptr)	create(dynamicModule->retrieveCorrespondingAnalysisEntry(analysisTitle.toStdString()));
	else							create(module, analysisName, analysisTitle);
}


int Analyses::_scriptRequestID = 0;

void Analyses::rCodeReturned(QString result, int requestId)
{
	if (_scriptIDMap.contains(requestId))
	{
		const QPair<Analysis*, QString>& pair = _scriptIDMap[requestId];
		pair.first->runScriptRequestDone(result, pair.second);
	}
	else
		Log::log()  << "Unkown Returned Rcode request ID " << requestId << std::endl;
}

void Analyses::sendRScriptHandler(Analysis* analysis, QString script, QString controlName)
{
	_scriptIDMap[_scriptRequestID] = qMakePair(analysis, controlName);

	emit sendRScript(script, _scriptRequestID++);
}

void Analyses::selectAnalysis(Analysis * analysis)
{
	for(size_t index=0; index<_orderedIds.size(); index++)
		if(_analysisMap[_orderedIds[index]] == analysis)
		{
			setCurrentAnalysisIndex(int(index));
			emit showAnalysisInResults(analysis->id());
			return;
		}
}

void Analyses::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;
	
	emit dataSetChanged();
}


void Analyses::setCurrentAnalysisIndex(int currentAnalysisIndex)
{
	if (_currentAnalysisIndex == currentAnalysisIndex)
		return;

	_currentAnalysisIndex = currentAnalysisIndex;
	emit currentAnalysisIndexChanged(_currentAnalysisIndex);

	if(_currentAnalysisIndex > -1 && _currentAnalysisIndex < _orderedIds.size())
	{
		int id = _orderedIds[_currentAnalysisIndex];
		emit analysisNameSelected(QString::fromStdString(get(id)->name()));
		setVisible(true);
	}
	else
		emit analysesUnselected();
}

void Analyses::analysisIdSelectedInResults(int id)
{
	for(size_t i=0; i<_orderedIds.size(); i++)
		if(_orderedIds[i] == id)
		{
			setCurrentAnalysisIndex(int(i));
			emit analysisSelectedIndexResults(int(i)); //Picked up in QML
			return;
		}
}

void Analyses::analysesUnselectedInResults()
{
	setCurrentAnalysisIndex(-1);
}

void Analyses::selectAnalysisAtRow(int row)
{
	setCurrentAnalysisIndex(row);
	if(row > -1)
		emit showAnalysisInResults(_orderedIds[row]);
}

void Analyses::unselectAnalysis()
{
	setCurrentAnalysisIndex(-1);
	emit unselectAnalysisInResults();
}


void Analyses::setCurrentFormHeight(double currentFormHeight)
{
	if (qFuzzyCompare(_currentFormHeight, currentFormHeight))
		return;

	_currentFormHeight = currentFormHeight;
	emit currentFormHeightChanged(_currentFormHeight);
}

void Analyses::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);

	if(!_visible)
		unselectAnalysis();
}

void Analyses::analysisTitleChanged(int id, QString title)
{
	Analysis * analysis = get(id);

	if(analysis != nullptr)
		analysis->setTitleQ(title);
}
