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
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "processinfo.h"
#include "modules/ribbonmodel.h"
#include "analysisform.h"

#include <QFile>
#include <QTimer>


#include "utils.h"
#include "tempfiles.h"
#include "log.h"

using namespace std;
using Modules::Upgrader;

Analyses * Analyses::_singleton = nullptr;

Analyses::Analyses()
	: QAbstractListModel(DataSetPackage::pkg())
{
	if(_singleton) throw std::runtime_error("Can only instantiate single copy of Analyses!");
	_singleton = this;

	connect(this,					&Analyses::requestComputedColumnDestruction,	this,	&Analyses::dataSetColumnsChanged	, Qt::QueuedConnection	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::dataSetChanged,				this,	&Analyses::dataSetChanged									);
	connect(DataSetPackage::pkg(),	&DataSetPackage::columnDataTypeChanged,			this,	&Analyses::dataSetColumnsChanged							);
	connect(DataSetPackage::pkg(),	&DataSetPackage::labelChanged,					this,	&Analyses::dataSetColumnsChanged							);
}



Analysis* Analyses::createFromJaspFileEntry(Json::Value analysisData, RibbonModel* ribbonModel)
{
	Analysis::Status status		= Analysis::parseStatus(analysisData["status"].asString());
	size_t id					= analysisData["id"].asUInt();

	if(_nextId <= id) _nextId = id + 1;

	Analysis				*	analysis = nullptr;
	Modules::UpgradeMsgs		msgs;
	bool						wasUpgraded = Upgrader::upgrader()->upgradeAnalysisData(analysisData, msgs);
	Json::Value				&	optionsJson	= analysisData["options"];

	if(analysisData.get("dynamicModule", Json::nullValue).isNull())
	{
		Json::Value	&	versionJson		= analysisData["version"];
		Version			version			= versionJson.isNull() ? AppInfo::version : Version(versionJson.asString());

		QString			name			= tq(analysisData["name"].asString()),
						module			= analysisData["module"].asString() != "" ? tq(analysisData["module"].asString()) : "Common",
						title			= tq(analysisData.get("title", "").asString());
		auto		*	analysisEntry	= ribbonModel->getAnalysis(module.toStdString(), name.toStdString());
		QString			qml				= analysisEntry ? tq(analysisEntry->qml()) : name + ".qml";


		if(title == "")
			title = analysisEntry ? tq(analysisEntry->title()) : name;
		
		analysis = create(module, name, qml, title, id, version, &optionsJson, status, false);

		analysis->loadExtraFromJSON(analysisData);
	}
	else
	{
		std::string title			= analysisData.get("title", "").asString();
		auto *	analysisEntry		= DynamicModules::dynMods()->retrieveCorrespondingAnalysisEntry(analysisData["dynamicModule"]);
				analysis			= create(analysisEntry, id, status, false, title, analysisData["dynamicModule"]["moduleVersion"].asString(), &optionsJson);
		auto *	dynMod				= analysisEntry->dynamicModule();

		if(!dynMod->loaded())
			dynMod->setLoadingNeeded();

	}

	analysis->setUserData(analysisData["userdata"]);
	analysis->setResults(analysisData["results"], status);

	if(wasUpgraded)
		analysis->setUpgradeMsgs(msgs);

	return analysis;
}

Analysis* Analyses::create(const QString &module, const QString &name, const QString& qml, const QString &title, size_t id, const Version &version, Json::Value *options, Analysis::Status status, bool notifyAll)
{
	Analysis *analysis = new Analysis(id, module.toStdString(), name.toStdString(), qml.toStdString(), title.toStdString(), version, options);
	storeAnalysis(analysis, id, notifyAll);
	bindAnalysisHandler(analysis);

	return analysis;
}

Analysis* Analyses::create(Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status, bool notifyAll, std::string title, std::string moduleVersion, Json::Value *options)
{
	Analysis *analysis = new Analysis(id, analysisEntry, title, moduleVersion, options);

	analysis->setResults(analysisEntry->getDefaultResults(), status);
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
	connect(analysis, &Analysis::statusChanged,						this, &Analyses::analysisStatusChanged				);
	connect(analysis, &Analysis::sendRScript,						this, &Analyses::sendRScriptHandler					);
	connect(analysis, &Analysis::titleChanged,						this, &Analyses::setChangedAnalysisTitle			);
	connect(analysis, &Analysis::imageSavedSignal,					this, &Analyses::analysisImageSaved					);
	connect(analysis, &Analysis::imageEditedSignal,					this, &Analyses::analysisImageEdited				);
	connect(analysis, &Analysis::requestColumnCreation,				this, &Analyses::requestColumnCreation				);
	connect(analysis, &Analysis::resultsChangedSignal,				this, &Analyses::analysisResultsChanged				);
	connect(analysis, &Analysis::requestComputedColumnCreation,		this, &Analyses::requestComputedColumnCreation,		Qt::DirectConnection);
	connect(analysis, &Analysis::requestComputedColumnDestruction,	this, &Analyses::requestComputedColumnDestruction,	Qt::DirectConnection);
	connect(analysis, &Analysis::titleChanged,						this, &Analyses::somethingModified					);

	
	if (Settings::value(Settings::DEVELOPER_MODE).toBool())
	{
		QString filePath = tq(analysis->qmlFormPath());
		
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
	setCurrentAnalysisIndex(-1);
	beginResetModel();
	_resultsMeta = Json::nullValue;
	_allUserData = Json::nullValue;

	for (auto & idAnalysis : _analysisMap)
	{
		Analysis* analysis = idAnalysis.second;
		idAnalysis.second  = nullptr;

		emit analysisRemoved(analysis);
		delete analysis;
	}

	_analysisMap.clear();
	_orderedIds.clear();

	_nextId = 0;
	endResetModel();
	emit countChanged();
}

void Analyses::reload(Analysis *analysis, bool logProblem)
{

	for (size_t i = 0; i < _orderedIds.size(); i++)
		if (_analysisMap[_orderedIds[i]] == analysis)
		{
			int ind = int(i);
			// Force the loader to load again the QML file
			beginRemoveRows(QModelIndex(), ind, ind);
			endRemoveRows();

			beginInsertRows(QModelIndex(), ind, ind);
			endInsertRows();

			return;
		}


	if(logProblem)
		Log::log() << "Analysis " << analysis->title() << " not found!" << std::endl;
}


bool Analyses::allFresh() const
{
	for (auto idAnalysis : _analysisMap)
		if (idAnalysis.second->needsRefresh())
			return false;

	return true;
}

void Analyses::_analysisQMLFileChanged(Analysis *analysis)
{
	emit emptyQMLCache();
	reload(analysis, false); //Do not log problem because it can cause trouble!
}

Json::Value Analyses::asJson() const
{
	Json::Value analysesJson		= Json::objectValue,
				analysesDataList	= Json::arrayValue;;

	applyToAll([&analysesDataList](const Analysis * analysis)
		{ analysesDataList.append(analysis->asJSON()); });

	analysesJson["analyses"]	= analysesDataList;
	analysesJson["meta"]		= resultsMeta();

	return analysesJson;
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

	QList<int> toRemove;
	QMapIterator<int, QPair<Analysis*, QString> > it(_scriptIDMap);
	while (it.hasNext())
	{
		it.next();
		if (it.value().first == analysis)
			toRemove.push_back(it.key());
	}

	beginRemoveRows(QModelIndex(), indexAnalysis, indexAnalysis);
	analysis->remove();
	_analysisMap.erase(id);
	_orderedIds.erase(_orderedIds.begin() + indexAnalysis);
	for (int requestId : toRemove)
		_scriptIDMap.remove(requestId);
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


void Analyses::refreshAnalysesUsingColumns(
		std::vector<std::string>			changedColumns,
		std::vector<std::string>			missingColumns,
		std::map<std::string, std::string>	changeNameColumns,
		std::vector<std::string>			oldColumnNames,
		bool								hasNewColumns)
{
	std::set<Analysis *> analysesToRefresh;
	std::set<Analysis *> analysesToRebind;

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
				{
					analysis->removeUsedVariable(varname);
					analysesToRebind.insert(analysis);
				}

			if (aNameChanged)
				for (std::string & varname : interChangename)
				{
					analysis->replaceVariableName(varname, changeNameColumns[varname]);
					analysesToRebind.insert(analysis);
				}

			if (aNameChanged || aColumnRemoved || aColumnChanged)
				analysesToRefresh.insert(analysis);
		}
	}

	for (Analysis *analysis : analysesToRefresh)
	{
		analysis->setRefreshBlocked(false);
		analysis->refresh();
	}

	for (Analysis *analysis : analysesToRebind)
		// replaceVariableName and removeUsedVariable just changes the options, not the form
		// So by rebinding the form with their options, it will update the form
		analysis->rebind();

	if (hasNewColumns || missingColumns.size() > 0 || changeNameColumns.size() > 0 || changedColumns.size() > 0)
		applyToAll([&](Analysis * a)
		{
			if (analysesToRebind.find(a) == analysesToRebind.end())
				// rebind already refreshes the available models together with the assigned models in the right way
				a->refreshAvailableVariablesModels();
		});

}


void Analyses::applyToSome(std::function<bool(Analysis *analysis)> applyThis)
{
	for(size_t id : _orderedIds)
		if(_analysisMap[id] != nullptr && !applyThis(_analysisMap[id]))
			return;
}

void Analyses::applyToAll(std::function<void(Analysis *analysis)> applyThis)
{
	for(size_t id : _orderedIds)
		if(_analysisMap[id] != nullptr)
			applyThis(_analysisMap[id]);
}

void Analyses::applyToAll(std::function<void(Analysis *analysis)> applyThis) const
{
	for(size_t id : _orderedIds)
		if(_analysisMap.at(id) != nullptr)
			applyThis(_analysisMap.at(id));
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
	case formPathRole:		return tq(analysis->qmlFormPath());
	case Qt::DisplayRole:
	case titleRole:			return tq(analysis->title());
	case nameRole:			return tq(analysis->name());
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

void Analyses::analysisClickedHandler(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module)
{
	Modules::DynamicModule * dynamicModule = DynamicModules::dynMods()->dynamicModule(module.toStdString());

	if(dynamicModule != nullptr)	create(dynamicModule->retrieveCorrespondingAnalysisEntry(fq(analysisFunction)));
	else							create(module, analysisFunction, analysisQML, analysisTitle);
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
		Log::log()  << "Unknown Returned Rcode request ID " << requestId << std::endl;
}

void Analyses::sendRScriptHandler(Analysis* analysis, QString script, QString controlName, bool whiteListedVersion)
{
	_scriptIDMap[_scriptRequestID] = qMakePair(analysis, controlName);

	emit sendRScript(script, _scriptRequestID++, whiteListedVersion);
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

void Analyses::setCurrentAnalysisIndex(int currentAnalysisIndex)
{
	if (_currentAnalysisIndex == currentAnalysisIndex)
		return;

	_currentAnalysisIndex = currentAnalysisIndex;
	emit currentAnalysisIndexChanged(_currentAnalysisIndex);

	if(_currentAnalysisIndex > -1 && _currentAnalysisIndex < _orderedIds.size())
		setVisible(true);
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

			if(!visible())
				setVisible(true);

			return;
		}
}

void Analyses::analysesUnselectedInResults()
{
	if (count() > 1)
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

	setCurrentFormPrevH(_currentFormHeight);
	_currentFormHeight = currentFormHeight;

	//std::cout << "cur form H: "<<_currentFormHeight << std::endl;
	emit currentFormHeightChanged(_currentFormHeight);
}

void Analyses::setCurrentFormPrevH(double currentFormPrevH)
{
	if (qFuzzyCompare(_currentFormPrevH, currentFormPrevH))
		return;

	_currentFormPrevH = currentFormPrevH;
	//std::cout << "cur form Prev H: "<<_currentFormPrevH << std::endl;

	emit currentFormPrevHChanged(_currentFormPrevH);
}

void Analyses::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);

	if(currentAnalysisIndex() != -1)
	{
		if(!_visible)		emit unselectAnalysisInResults();
		else				emit showAnalysisInResults(_orderedIds[currentAnalysisIndex()]);
	}
}

//Called from Enter in AnalysisFormExpander.qml
void Analyses::move(int fromIndex, int toIndex)
{
	int size = int(_orderedIds.size());
	if (fromIndex < 0 || toIndex < 0)
	{
		Log::log() << "Index in Analyses swaping negative!" << std::flush;
		return;
	}
	if (fromIndex >= size || toIndex >= size)
	{
		Log::log() << "Index in Analyses swaping too big: " << fromIndex << ", " << toIndex << ", size: " << _orderedIds.size();
		return;
	}
	if (fromIndex == toIndex)
		return;

	size_t fromId = _orderedIds[size_t(fromIndex)];
	if (beginMoveRows(QModelIndex(), fromIndex, fromIndex, QModelIndex(), toIndex > fromIndex ? (toIndex + 1) : toIndex))
	{
		_orderedIds.erase(_orderedIds.begin() + fromIndex);
		_orderedIds.insert(_orderedIds.begin() + toIndex, fromId);
		endMoveRows();
	}
}

void Analyses::setMoving(bool moving)
{
	if (_moving == moving)
		return;

	_moving = moving;

	if (moving)
		_orderedIdsBeforeMoving = _orderedIds;

	emit movingChanged(_moving);
}

//Called after setting moving to false on end of drag in AnalysisFormExpander.qml
Analysis* Analyses::getAnalysisBeforeMoving(size_t index)
{
	if (index < _orderedIdsBeforeMoving.size())
		return _analysisMap.at(_orderedIdsBeforeMoving[index]);

	return nullptr;
}


void Analyses::moveAnalysesResults(Analysis* fromAnalysis, int index)
{
	Analysis* toAnalysis = getAnalysisBeforeMoving(size_t(index));

	if (fromAnalysis && toAnalysis && fromAnalysis != toAnalysis)
		emit moveAnalyses(fromAnalysis->id(), toAnalysis->id());
}

void Analyses::analysisTitleChangedInResults(int id, QString title)
{
	Analysis * analysis = get(id);

	if(analysis != nullptr)
		analysis->setTitleQ(title);
}

void Analyses::setChangedAnalysisTitle()
{
    Analysis * analysis = dynamic_cast<Analysis*>(QObject::sender());

    if (analysis != nullptr)
        emit analysisTitleChanged(analysis);
}

void Analyses::refreshAvailableVariables()
{
	applyToAll([](Analysis * a) { a->refreshAvailableVariablesModels();	});
}

void Analyses::duplicateAnalysis(size_t id)
{
	if(!get(id)) return;

	Analysis	* original = get(id),
				* analysis = new Analysis(++_nextId, original);

	storeAnalysis(analysis, analysis->id(), true);
	bindAnalysisHandler(analysis);
	analysis->emitDuplicationSignals();

	if(analysis->status() != Analysis::Status::Complete)
		analysis->refresh();
}

void Analyses::showDependenciesInAnalysis(size_t analysis_id, QString optionName)
{
	if(!get(analysis_id)) return;

	get(analysis_id)->showDependenciesOnQMLForObject(optionName);
}

void Analyses::analysisTitleChangedHandler(string moduleName, string oldTitle, string newTitle)
{
	applyToAll([&](Analysis * a)
	{
		if (a->module() == moduleName && a->title() == oldTitle)
			a->setTitle(newTitle);
	});
}

void Analyses::languageChangedHandler()
{
	refreshAllAnalyses();
	applyToAll([&](Analysis * a)
	{
		emit a->form()->languageChanged();
	});

	emit setResultsMeta(tq(_resultsMeta.toStyledString()));
}

void Analyses::resultsMetaChanged(QString json)
{
	Json::Reader().parse(fq(json), _resultsMeta);
}

void Analyses::allUserDataChanged(QString json)
{
	Json::Reader().parse(fq(json), _allUserData);
	setAnalysesUserData(_allUserData);
}
