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
#include "tempfiles.h"
#include "gui/jaspConfiguration/jaspconfiguration.h"
#include "modules/ribbonmodel.h"
#include "analysisform.h"
#include "knownissues.h"
#include <QTimer>
#include <QFile>
#include <QDir>
#include <QFileInfo>
#include <QSet>
#include <QCoreApplication>
#include <QEventLoop>
#include <QRegularExpression>
#include <json/value.h>
#include "log.h"
#include "rpc/jasprpcdispatcher.h"
#include "ai/agentstatetracker.h"
#include <vector>

using namespace std;
using Modules::Upgrader;

Analyses * Analyses::_singleton = nullptr;

Analyses::Analyses()
	: QAbstractListModel(DataSetPackage::pkg())
{
	if(_singleton) throw std::runtime_error("Can only instantiate single copy of Analyses!");
	_singleton = this;

	registerRpcHandlers();

	new KnownIssues(this);
}

void Analyses::destroyAllForms()
{
	Log::log() << "Analyses::destroyAllForms()" << std::endl;

	//Destroy all existing forms *before* destroying the rest of QML, to avoid a massive slew of errors
	applyToAll([](Analysis * a){ if(a->form()) a->destroyForm(); });
}


Analysis* Analyses::createFromJaspFileEntry(Json::Value analysisData, RibbonModel* ribbonModel)
{
	Log::log() << "Analyses::createFromJaspFileEntry" << std::endl;

	Analysis::Status status		= Analysis::parseStatus(analysisData["status"].asString());
	size_t id					= analysisData["id"].asUInt();

	//If the user saved an analysis that didnt crash and didnt complete it should probably still be ran
	if(status != Analysis::Status::Complete && status != Analysis::Status::FatalError)
		status = Analysis::Status::Empty;

	if(_nextId <= id) _nextId = id + 1;


	Modules::UpgradeMsgs		msgs;
	bool						wasUpgraded		= Upgrader::upgrader()->upgradeAnalysisData(DynamicModules::dynMods()->modules(), analysisData, msgs);
	Json::Value				&	optionsJson		= analysisData["options"];
	std::string					title			= analysisData.get("title", "").asString();

	// Reports have no module — create via report constructor, no module resolution needed
	if (analysisData.get("isReport", false).asBool())
	{
		Analysis *report = new Analysis(id, title);
		report->checkDefaultTitleFromJASPFile(analysisData);
		storeAnalysis(report, id, false);
		bindAnalysisHandler(report);
		report->loadResultsUserdataAndRSourcesFromJASPFile(analysisData, status);
		return report;
	}

	Modules::AnalysisEntry	*	analysisEntry	= DynamicModules::dynMods()->retrieveCorrespondingAnalysisEntry(analysisData["dynamicModule"]);

	if (!analysisEntry)
	{
		Log::log() << "Analyses::createFromJaspFileEntry: Could not resolve module for analysis '" << title << "' (id=" << id << ") — skipping." << std::endl;
		return nullptr;
	}

	Analysis				*	analysis		= create(analysisData, analysisEntry, id, status, false, title, analysisData["dynamicModule"]["moduleVersion"].asString(), optionsJson);

	if(msgs.count(Modules::analysisLog))
	{
		QStringList msgAna = tq(msgs[Modules::analysisLog]);
		analysis->setErrorInResults(fq(msgAna.join("\n")));
	}

	if(wasUpgraded)
		analysis->setUpgradeMsgs(msgs);

	if(!TempFiles::stateFileExists(id))
		analysis->_storedWithoutState = true; //This will trigger the "you need a refresh" on resize

	for(const Json::Value & columnName : analysisData.get("columns", Json::arrayValue))
	{
		Column * col = DataSetPackage::pkg()->dataSet()->column(columnName.asString());

		if(		col
			&&
			(	col->codeType() == computedColumnType::analysisNotComputed
			||	col->codeType() == computedColumnType::notComputed			)
			&&	col->analysisId() == -1)
			col->setAnalysisId(analysis->id());
	}

	return analysis;
}

Analysis* Analyses::create(Modules::AnalysisEntry * analysisEntry, const Json::Value & options)
{
	return create(Json::nullValue, analysisEntry, _nextId++, Analysis::Empty, true, "", "", options);
}

Analysis* Analyses::create(const Json::Value & analysisData, Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status, bool notifyAll, const std::string & title, const Version & optionsVersion, const Json::Value & options)
{
	Analysis *analysis = new Analysis(id, analysisEntry, title, optionsVersion, options);

	analysis->checkDefaultTitleFromJASPFile(analysisData);

	storeAnalysis(analysis, id, notifyAll);
	bindAnalysisHandler(analysis);

	if(!analysisData.isNull())	analysis->loadResultsUserdataAndRSourcesFromJASPFile(analysisData, status);
	else						analysis->setResults(analysisEntry->getDefaultResults(), status);



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
		emit showAnalysisInResults(id);
	}
}

void Analyses::bindAnalysisHandler(Analysis* analysis)
{
	connect(analysis,	&Analysis::userModifiedSomething,				this, [](){ DataSetPackage::pkg()->setModified(true); });
	connect(analysis,	&Analysis::statusChanged,						this, &Analyses::analysisStatusChanged				);
	connect(analysis,	&Analysis::sendRScriptSignal,					this, &Analyses::sendRScriptHandler					);
	connect(analysis,	&Analysis::sendFilterSignal,					this, &Analyses::sendFilterHandler					);
	connect(analysis,	&Analysis::titleChanged,						this, &Analyses::setChangedAnalysisTitle			);
	connect(analysis,	&Analysis::imageSavedSignal,					this, &Analyses::analysisImageSaved					);
	connect(analysis,	&Analysis::imageEditedSignal,					this, &Analyses::analysisImageEdited				);
	connect(analysis,	&Analysis::requestColumnCreation,				this, &Analyses::requestColumnCreation				);
	connect(analysis,	&Analysis::resultsChangedSignal,				this, &Analyses::analysisResultsChanged				);
	connect(analysis,	&Analysis::requestComputedColumnCreation,		this, &Analyses::requestComputedColumnCreation,		Qt::DirectConnection);
	connect(analysis,	&Analysis::requestComputedColumnDestruction,	this, &Analyses::requestComputedColumnDestruction,	Qt::DirectConnection);
	connect(analysis,	&Analysis::titleChanged,						this, &Analyses::somethingModified					);
	connect(analysis,	&Analysis::imageChanged,						this, &Analyses::somethingModified					);
	connect(analysis,	&Analysis::userDataChangedSignal,				this, &Analyses::analysisOverwriteUserdata			);
	connect(analysis,	&Analysis::emptyQMLCache,						this, &Analyses::emptyQMLCache						);
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
		analysis->remove();

		emit analysisRemoved(analysis);
		delete analysis;
	}

	_analysisMap.clear();
	_orderedIds.clear();

	_nextId = 0;
	endResetModel();
	emit countChanged();
}

void Analyses::reload(Analysis *analysis, bool qmlFileNameChanged, bool logProblem)
{

	for (size_t i = 0; i < _orderedIds.size(); i++)
		if (_analysisMap[_orderedIds[i]] == analysis)
		{
			int ind = int(i);
			if(!qmlFileNameChanged)	Log::log() << "Analyses::reload(" << analysis << ") Force a reload of QML file '" << analysis->qmlFormPath() << "' for " << analysis->name() << "("<< analysis->id() << ")." << std::endl;
			else					Log::log() << "Analyses::reload(" << analysis << ") emit dataChanged for QML file '" << analysis->qmlFormPath() << "' for " << analysis->name() << "("<< analysis->id() << ")." << std::endl;

			if(!qmlFileNameChanged)	analysis->reloadForm();
			else					emit dataChanged(index(ind), index(ind), QList<int>({ Qt::DisplayRole, formPathRole}));

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


bool Analyses::allFinished() const
{
	for (auto idAnalysis : _analysisMap)
		if (!idAnalysis.second->isFinished())
			return false;

	return true;
}

Json::Value Analyses::asJson() const
{
	Json::Value analysesJson		= Json::objectValue,
				analysesDataList	= Json::arrayValue;;

	applyToAll([&analysesDataList](const Analysis * analysis)
		{ analysesDataList.append(analysis->asJSON(true)); });

	analysesJson["analyses"]	= analysesDataList;
	analysesJson["meta"]		= resultsMeta();

	return analysesJson;
}

void Analyses::saveAnalysesJsonForReload()
{
	beginResetModel();

	_tempSave = asJson();

	destroyAllForms();

	for(auto & idAnalysis : _analysisMap)
		delete idAnalysis.second;

	_analysisMap.clear();
	_orderedIds.clear();

	endResetModel();
}

void Analyses::reloadSavedAnalysesJson()
{
	if(!_tempSave.isObject() || !_tempSave.isMember("analyses") || !_tempSave.isMember("meta"))
		return;

	beginResetModel();

	bool errorFound;
	std::stringstream errors;
	loadAnalysesFromJaspFileJson(_tempSave["analyses"], _tempSave["meta"], errorFound, errors, RibbonModel::singleton());

	//applyToAll([](Analysis * a){ a->setBeingTranslated(true); });
	endResetModel();

	_tempSave = Json::nullValue;
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
	emit somethingModified();

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
	Log::log() << "void Analyses::refreshAnalysesOfDynamicModule(" << module->toString() << ")" << std::endl;

	for(auto & keyval : _analysisMap)
		if(keyval.second->dynamicModule() == module)
			keyval.second->refresh();
}


void Analyses::replaceAnalysesOfDynamicModule(Modules::DynamicModule * oldModule, Modules::DynamicModule * newModule)
{
	Log::log() << "void Analyses::replaceAnalysesOfDynamicModule(" << oldModule->toString() << ", " <<  newModule->toString() << ")" << std::endl;

	for(auto & keyval : _analysisMap)
	{
		if(keyval.second->dynamicModule() != oldModule && keyval.second->dynamicModule()->name() == newModule->name())
			Log::log() << "Replacing dynamic module of analyses but found one that uses same name but is not the old module..." << std::endl;

		if(keyval.second->dynamicModule() == oldModule)
			keyval.second->setDynamicModule(newModule);
	}
}

void Analyses::rescanAnalysisEntriesOfDynamicModule(Modules::DynamicModule * module)
{
	std::set<int> removeIds;
	for(auto & keyval : _analysisMap)
		if(keyval.second->dynamicModule() == module)
		{
			if(!keyval.second->checkAnalysisEntry()) // Check if the analysisEntry this analysis is based still exists
				removeIds.insert(keyval.first);
			else
			{
				Analysis * a = keyval.second;

				//This function is called once after 300 ms upon translation due to delayedUpdate in description
				//and causes the set analysis options to be cleared without this additional flag check set at the start of translations
				if(a->readyToCreateForm() && !a->beingTranslated())
				{
					a->createForm();
				}
				a->setBeingTranslated(false);
			}
		}

	for(const int & id : removeIds)
		removeAnalysisById(size_t(id));
}

void Analyses::reloadQmlAnalysesDynamicModule(Modules::DynamicModule * module)
{
	for(auto idAnalysis : _analysisMap)
		if(idAnalysis.second->dynamicModule() == module)
			idAnalysis.second->analysisQMLFileChanged();
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

void Analyses::loadAnalysesFromDatasetPackage(bool & errorFound, stringstream & errorMsg, RibbonModel * ribbonModel)
{
	if (DataSetPackage::pkg()->hasAnalyses())
	{
		Json::Value analysesData = DataSetPackage::pkg()->analysesData();

		if (analysesData.isNull())
		{
			errorFound = true;
			errorMsg << "An error has been detected and analyses could not be loaded.";
			return;
		}

		Json::Value meta, analysesDataList;
		if (!analysesData.isArray())
		{
			analysesDataList	= analysesData.get("analyses",	analysesData);
			meta				= analysesData.get("meta",		Json::nullValue);

			if (!meta.isNull())
			{
				QString results = tq(analysesData["meta"].toStyledString());
				resultsMetaChanged(results);
				emit setResultsMeta(results);
			}
		}

		loadAnalysesFromJaspFileJson(analysesDataList, meta, errorFound, errorMsg, ribbonModel);
	}
}

void Analyses::loadAnalysesFromJaspFileJson(const Json::Value & analysesDataList, const Json::Value & meta, bool & errorFound, stringstream & errorMsg, RibbonModel * ribbonModel)
{
	int				corruptAnalyses = 0;
	stringstream	corruptionStrings;

	Log::log() << "Loading analyses from jasp-file, entering loop." << std::endl;

	//There is no point trying to show progress here because qml is not updated while this function runs...
	for (const Json::Value & analysisData : analysesDataList)
	{
		try
		{
			createFromJaspFileEntry(analysisData, ribbonModel);
		}
		catch (Modules::ModuleException modProb)
		{
			//Maybe show a nicer messagebox?
			errorFound = true;
			corruptionStrings << "\n" << (++corruptAnalyses) << ": " << modProb.what();

			Log::log() << "Caught module exception: " << modProb.what() << std::endl;
		}
		catch (runtime_error & e)
		{
			errorFound = true;
			corruptionStrings << "\n" << (++corruptAnalyses) << ": " << e.what();

			Log::log() << "Caught runtime_error exception: " << e.what() << std::endl;
		}
		catch (exception & e)
		{
			errorFound = true;
			corruptionStrings << "\n" << (++corruptAnalyses) << ": " << e.what();

			Log::log() << "Caught exception: " << e.what() << std::endl;
		}
	}

	if (corruptAnalyses == 1)			errorMsg << "An error was detected in an analysis. This analysis has been removed for the following reason:\n" << corruptionStrings.str();
	else if (corruptAnalyses > 1)		errorMsg << "Errors were detected in " << corruptAnalyses << " analyses. These analyses have been removed for the following reasons:\n" << corruptionStrings.str();
	else								Log::log() << "Loading analyses seems to have worked out fine." << std::endl;
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

Analysis* Analyses::createAnalysis(const QString& module, const QString& analysis)
{
	Modules::DynamicModule * dynamicModule = DynamicModules::dynMods()->dynamicModule(module.toStdString());
	Json::Value options = JASPConfiguration::getInstance()->getAnalysisOptionValues(module, analysis);

	if (dynamicModule)
		return create(dynamicModule->retrieveCorrespondingAnalysisEntry(fq(analysis)), options);
	else
		return nullptr;

}

Analysis* Analyses::createReport(const std::string& title)
{
	Analysis *analysis = new Analysis(_nextId++, title);

	storeAnalysis(analysis, analysis->id(), true);
	bindAnalysisHandler(analysis);

	return analysis;
}

void Analyses::analysisClickedHandler(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module)
{
	createAnalysis(module, analysisFunction);
}


int Analyses::_scriptRequestID = 0;

void Analyses::rCodeReturned(QString result, int requestId, bool hasError)
{
	if(requestId == -1)
		return;//Not for us

	if (_scriptIDMap.contains(requestId))
	{
		const QPair<Analysis*, QString>& pair = _scriptIDMap[requestId];
		pair.first->runScriptRequestDone(result, pair.second, hasError);
	}
	else
		Log::log()  << "Unknown Returned Rcode request ID " << requestId << std::endl;
}

void Analyses::filterByNameDone(QString name, QString error)
{
	applyToAll([&](Analysis * a)
	{
		a->filterByNameDone(name, error);
	});
}

void Analyses::sendRScriptHandler(QString script, QString controlName, bool whiteListedVersion, QString module)
{
	Analysis* analysis = qobject_cast<Analysis*>(sender());
	_scriptIDMap[_scriptRequestID] = qMakePair(analysis, controlName);

	emit sendRScript(script, _scriptRequestID++, whiteListedVersion, module);
}

void Analyses::sendFilterHandler(QString name, QString module)
{
	emit sendFilterByName(name, module);
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

void Analyses::showRSyntaxInResults(bool show)
{
	DataSetPackage::pkg()->setWorkspaceShowRSyntax(show);

	applyToAll([&](Analysis * a)
	{
		a->setRSyntaxTextInResult(show);
	});
}

void Analyses::analysisTitleChangedInResults(int id, QString title)
{
	Analysis * analysis = get(id);

	if(analysis != nullptr)
		analysis->setTitle(fq(title));
}

void Analyses::setChangedAnalysisTitle()
{
	Analysis * analysis = dynamic_cast<Analysis*>(QObject::sender());

	if (analysis != nullptr)
		emit analysisTitleChanged(analysis);
}

Analysis* Analyses::duplicateAnalysis(size_t id, bool isReport)
{
	if(!get(id)) return nullptr;

	Analysis	* original = get(id),
				* analysis = new Analysis(++_nextId, original);

	if (isReport)
		analysis->setReport(true);

	storeAnalysis(analysis, analysis->id(), true);
	bindAnalysisHandler(analysis);
	analysis->emitDuplicationSignals();

	analysis->refresh();
	return analysis;
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

void Analyses::prepareForLanguageChange()
{
	applyToAll([&](Analysis * a)
	{
		a->setBeingTranslated(true);
		a->setRefreshBlocked(true);

		if(!a->isFinished())
			a->abort();
	});
}


void Analyses::languageChangedHandler()
{
	applyToAll([&](Analysis * a)
	{
		a->setRefreshBlocked(false);
		emit a->form()->languageChanged();
	});
	refreshAllAnalyses();
	emit setResultsMeta(tq(_resultsMeta.toStyledString()));
}

void Analyses::dataModeChanged(bool dataMode)
{
	applyToAll([&](Analysis * a)
	{
		if(dataMode && !a->isFinished())
			a->refresh();
	});
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

// ---- RPC handler helpers ----------------------------------------------------

Analysis* Analyses::_rpcResolveAnalysis(int analysisId, Json::Value& errorResponse)
{
	Analysis* a = Analyses::analyses()->get(static_cast<size_t>(analysisId));
	if (!a)
		errorResponse = JaspRpcDispatcher::errorResult(
			"Analysis not found: " + std::to_string(analysisId));
	return a;
}

void Analyses::_rpcWriteIdentity(Json::Value& response, Analysis* a)
{
	response["analysisId"] = static_cast<int>(a->id());
	response["module"]     = a->module();
	response["analysis"]   = a->name();
}

void Analyses::_rpcWriteStatus(Json::Value& response, Analysis* a)
{
	response["status"] = Analysis::statusToString(a->status());
}

void Analyses::_rpcWriteOptions(Json::Value& response, Analysis* a, bool includeDesc)
{
	response["options"]    = a->boundValues();
	response["optionMeta"] = a->form() ? a->form()->optionMeta(includeDesc) : Json::Value(Json::objectValue);
}

/// Iterative diff: collect flat (path, value) pairs then reconstruct the tree.
/// Avoids stack overflow on deeply nested schemas.
/// Deleted keys are signalled with a null value in the delta.
/// Returns Json::nullValue when there are no changes.
static Json::Value _diffOptionMeta(const Json::Value& oldMeta, const Json::Value& newMeta)
{
	// Either side isn't an object (null, etc.) → wholesale replacement
	if (!oldMeta.isObject() || !newMeta.isObject())
		return newMeta;

	// ---- Pass 1: DFS with explicit stack → collect flat (path, value) pairs ----
	struct Diff { std::vector<std::string> path; Json::Value value; };
	std::vector<Diff> diffs;

	struct Frame { std::vector<std::string> path; const Json::Value* oldVal; const Json::Value* newVal; };
	std::vector<Frame> stack;
	stack.push_back({{}, &oldMeta, &newMeta});

	while (!stack.empty())
	{
		Frame f = std::move(stack.back());
		stack.pop_back();

		const Json::Value& ov = *f.oldVal;
		const Json::Value& nv = *f.newVal;

		// Keys in new but not old → addition
		// Keys in both objects → push frame to recurse
		// Keys changed (leaf) → diff
		for (const auto& key : nv.getMemberNames())
		{
			auto childPath = f.path;  childPath.push_back(key);

			if (!ov.isMember(key))
				diffs.push_back({std::move(childPath), nv[key]});              // added
			else if (nv[key].isObject() && ov[key].isObject())
				stack.push_back({std::move(childPath), &ov[key], &nv[key]});  // recurse
			else if (nv[key] != ov[key])
				diffs.push_back({std::move(childPath), nv[key]});              // changed
		}

		// Keys in old but not new → deletion, signal with null
		for (const auto& key : ov.getMemberNames())
			if (!nv.isMember(key))
			{
				auto childPath = f.path;  childPath.push_back(key);
				diffs.push_back({std::move(childPath), Json::nullValue});      // deleted
			}
	}

	if (diffs.empty()) return Json::nullValue;

	// ---- Pass 2: reconstruct tree from flat paths ----
	Json::Value delta(Json::objectValue);
	for (auto& d : diffs)
	{
		Json::Value* node = &delta;
		for (size_t i = 0; i + 1 < d.path.size(); i++)
		{
			const std::string& seg = d.path[i];
			if (!node->isMember(seg))
				(*node)[seg] = Json::Value(Json::objectValue);
			node = &(*node)[seg];
		}
		(*node)[d.path.back()] = std::move(d.value);
	}

	return delta;
}

void Analyses::_rpcWriteOptionsDelta(Json::Value& response, Analysis* a, bool includeDesc, bool forceFull)
{
	response["options"] = a->boundValues();

	Json::Value fullMeta = a->form() ? a->form()->optionMeta(includeDesc) : Json::Value(Json::objectValue);

	if (forceFull)
	{
		response["optionMeta"] = fullMeta;
		a->_lastSentMeta        = fullMeta;
		return;
	}

	Json::Value delta = _diffOptionMeta(a->_lastSentMeta, fullMeta);
	a->_lastSentMeta = fullMeta;

	if (!delta.isNull())
		response["optionMetaDelta"] = delta;
	// else: nothing changed, omit entirely
}

void Analyses::writeOptionsDelta(Json::Value& entry, Analysis* a, bool includeDesc, bool useDelta)
{
	_rpcWriteOptionsDelta(entry, a, includeDesc, !useDelta);
}

void Analyses::_rpcWriteFinishedResults(Json::Value& response, Analysis* a, int analysisId)
{
	Json::Value results = a->results();
	stripResults(results);
	response["results"]       = results;
	response["jaspResultsRds"] = TempFiles::analysisResourcePath(analysisId, "jaspResults.rds");
}

void Analyses::stripResults(Json::Value& val)
{
	std::vector<Json::Value*> stack;
	stack.push_back(&val);

	while (!stack.empty())
	{
		Json::Value* node = stack.back();
		stack.pop_back();

		if (node->isObject())
		{
			node->removeMember("editOptions");
			for (auto& member : node->getMemberNames())
				stack.push_back(&(*node)[member]);
		}
		else if (node->isArray())
		{
			for (auto& entry : *node)
				stack.push_back(&entry);
		}
	}
}

// ---- composeResultJSON: reusable result composition ---------------------------------

Json::Value Analyses::composeResultJSON(const Json::Value& elements, int defaultSourceId, Json::Value& errorOut)
{
	// Validate the default source analysis exists and has results (unless -1, meaning no default)
	Json::Value defaultResults;
	bool hasDefaultSource = (defaultSourceId >= 0);

	if (hasDefaultSource)
	{
		Analysis* defaultSource = Analyses::analyses()->get(static_cast<size_t>(defaultSourceId));
		if (!defaultSource)
		{
			errorOut = JaspRpcDispatcher::errorResult(
				"Default source analysis not found: " + std::to_string(defaultSourceId));
			return Json::nullValue;
		}

		defaultResults = defaultSource->results();
		if (defaultResults.isNull() || !defaultResults.isMember(".meta"))
		{
			errorOut = JaspRpcDispatcher::errorResult(
				"Default source analysis " + std::to_string(defaultSourceId) +
				" has no results — run it first");
			return Json::nullValue;
		}
	}

	// --- Recursive helper: find an element by name in the results tree ---
	std::function<Json::Value(const Json::Value& results, const std::string& targetName)> findElement;
	findElement = [&findElement](const Json::Value& results, const std::string& targetName) -> Json::Value
	{
		if (!results.isMember(".meta"))
			return Json::nullValue;

		const Json::Value& meta = results[".meta"];
		for (const auto& entry : meta)
		{
			std::string name = entry.get("name", "").asString();
			if (name == targetName)
			{
				Json::Value result(Json::objectValue);
				result["meta"] = entry;
				if (results.isMember(name))
					result["data"] = results[name];
				return result;
			}

			std::string type = entry.get("type", "").asString();
			if (type == "collection" && results.isMember(name))
			{
				const Json::Value& collData = results[name];
				if (collData.isMember("collection"))
				{
					Json::Value collResults = collData["collection"];
					if (entry.isMember("meta"))
						collResults[".meta"] = entry["meta"];
					Json::Value found = findElement(collResults, targetName);
					if (!found.isNull())
						return found;
				}
			}
		}
		return Json::nullValue;
	};

	// --- Build composed results ---
	Json::Value composed(Json::objectValue);
	Json::Value newMeta(Json::arrayValue);
	int mdTextCounter = 0;

	for (const auto& el : elements)
	{
		if (el.isMember("md_text"))
		{
			std::string content = el.get("md_text", "").asString();
			if (content.empty())
			{
				errorOut = JaspRpcDispatcher::errorResult(
					"Each 'md_text' element requires a non-empty 'md_text' field with the "
					"Markdown/HTML content. "
					"Example: { \"md_text\": \"# Summary\\n\\nThe groups do not differ significantly...\" }");
				return Json::nullValue;
			}

			std::string mdName = "_md_text_" + std::to_string(mdTextCounter++);
			Json::Value mdMeta(Json::objectValue);
			mdMeta["name"] = mdName;
			mdMeta["type"] = "md_text";
			newMeta.append(mdMeta);

			Json::Value mdData(Json::objectValue);
			mdData["name"]    = mdName;
			mdData["content"] = content;
			mdData["status"]  = "complete";
			composed[mdName] = mdData;
		}
		else if (el.isMember("name"))
		{
			int sourceId = el.get("sourceAnalysisId", defaultSourceId).asInt();

			// If the element falls back to a default source that doesn't exist, require explicit sourceAnalysisId
			if (sourceId == defaultSourceId && !hasDefaultSource)
			{
				errorOut = JaspRpcDispatcher::errorResult(
					"Element '" + el["name"].asString() + "' has no sourceAnalysisId. "
					"Reports have no default source — every named element must specify sourceAnalysisId.");
				return Json::nullValue;
			}

			const Json::Value* sourceResults = nullptr;
			std::string sourceTag;

			if (sourceId == defaultSourceId && hasDefaultSource)
			{
				sourceResults = &defaultResults;
				sourceTag = "analysis " + std::to_string(defaultSourceId);
			}
			else
			{
				Analysis* source = Analyses::analyses()->get(static_cast<size_t>(sourceId));
				if (!source)
				{
					errorOut = JaspRpcDispatcher::errorResult(
						"Source analysis not found for sourceAnalysisId: " +
						std::to_string(sourceId));
					return Json::nullValue;
				}
				const Json::Value& sr = source->results();
				if (sr.isNull() || !sr.isMember(".meta"))
				{
					errorOut = JaspRpcDispatcher::errorResult(
						"Source analysis " + std::to_string(sourceId) +
						" has no results — run it first");
					return Json::nullValue;
				}
				sourceResults = &sr;
				sourceTag = "analysis " + std::to_string(sourceId);
			}

			std::string name = el["name"].asString();
			Json::Value found = findElement(*sourceResults, name);
			if (found.isNull())
			{
				errorOut = JaspRpcDispatcher::errorResult(
					"Element not found in " + sourceTag + " results: '" + name + "'");
				return Json::nullValue;
			}

			newMeta.append(found["meta"]);
			composed[name] = found["data"];
		}
		else
		{
			errorOut = JaspRpcDispatcher::errorResult(
				"Each element must have either 'name' (to reference an existing result element) "
				"or 'md_text' (to insert a Markdown block). "
				"Examples: { \"name\": \"ttest\" } or { \"md_text\": \"# Summary\" }");
			return Json::nullValue;
		}
	}

	// --- Copy over any top-level keys that aren't element data (only when there is a default source)
	if (hasDefaultSource)
	{
		for (const auto& key : defaultResults.getMemberNames())
		{
			if (key == ".meta") continue;
			if (!composed.isMember(key))
				composed[key] = defaultResults[key];
		}
	}

	composed[".meta"] = newMeta;
	return composed;
}

// ---- RPC method registrations ----------------------------------------------

void Analyses::registerRpcHandlers()
{
	auto* disp = JaspRpcDispatcher::singleton();
	if (!disp)
		return;

	// Looked up by name from the RPCSpec.json registry.
	// Spec defines params & result schema — validated automatically.
	disp->registerMethodByName("analysis_create", [](const Json::Value& params) -> Json::Value
	{
		QString module   = QString::fromStdString(params["module"].asString());
		QString analysis = QString::fromStdString(params["analysis"].asString());

		Analysis* a = Analyses::analyses()->createAnalysis(module, analysis);
		if (!a)
			return JaspRpcDispatcher::errorResult(
				"Failed to create analysis: " + module.toStdString() +
				"::" + analysis.toStdString());

		// Mark AI-created analyses in the title
		a->setTitle(a->title() + " (AI)");

		Json::Value response = JaspRpcDispatcher::successResult();
		_rpcWriteIdentity(response, a);
		// Status defaults to "success" from successResult()
		_rpcWriteOptions(response, a, true);
		// Seed the delta baseline with the stripped version (no descriptions)
		// so the very first analysis_run only sends actual shape changes.
		a->_lastSentMeta = a->form() ? a->form()->optionMeta(false) : Json::Value(Json::objectValue);

		// Agent just observed this analysis's full state — clear dirty flags
		AgentStateTracker::notifyAnalysisObserved(a->id());

		return response;
	});

	disp->registerMethodByName("analysis_run", [](const Json::Value& params) -> Json::Value
	{
		int analysisId = params["analysisId"].asInt();

		Json::Value error;
		Analysis* a = _rpcResolveAnalysis(analysisId, error);
		if (!a) return error;

		AnalysisForm* form = a->form();
		if (!form)
			return JaspRpcDispatcher::errorResult(
				"Analysis form not available for analysis " + std::to_string(analysisId));

		if (params.isMember("relaxInputConstraints"))
			form->setRelaxInputConstraints(params["relaxInputConstraints"].asBool());

		Json::Value parsedOptions;
		std::string errorMsg;
		std::string rawOptions = Json::writeString(Json::StreamWriterBuilder(), params["options"]);

		form->parseOptions(rawOptions, parsedOptions, errorMsg);

		QString formErrors = form->errors();
		if (!formErrors.isEmpty())
		{
			if (!errorMsg.empty()) errorMsg += ", ";
			errorMsg += fq(formErrors);
		}

		if (!errorMsg.empty())
			return JaspRpcDispatcher::errorResult(
				"Validation errors on analysis options: " + errorMsg);

		// If the analysis was already running or completed with stale
		// options (e.g. EngineSync picked up the default/empty options
		// before analysis_run arrived), reset to Empty so it re-runs
		// with the options just set by parseOptions.  Bump revision
		// so any in-flight reply from the old run is rejected as stale.
		a->incrementRevision();
		a->run();

		bool wait      = params.get("wait", true).asBool();
		int  timeoutMs = params.get("timeoutMs", 30000).asInt();

		bool useDelta  = params.get("optionMetaDelta", true).asBool();

		// Fast path: results already ready
		if (a->isFinished())
		{
			Json::Value response = JaspRpcDispatcher::successResult();
			_rpcWriteIdentity(response, a);
			_rpcWriteStatus(response, a);
			_rpcWriteOptionsDelta(response, a, false, !useDelta);
			_rpcWriteFinishedResults(response, a, analysisId);
			AgentStateTracker::notifyAnalysisObserved(a->id());
			return response;
		}

		// Not waiting: return immediately with running status
		if (!wait)
		{
			Json::Value response = JaspRpcDispatcher::successResult();
			_rpcWriteIdentity(response, a);
			response["status"] = "running";
			_rpcWriteOptionsDelta(response, a, false, !useDelta);
			AgentStateTracker::notifyAnalysisObserved(a->id());
			return response;
		}

		// Blocking wait for R engine to finish
		JaspRpcDispatcher::waitAndProcessEvents(timeoutMs,
			[&](QEventLoop& loop, QTimer&) {
				QObject::connect(a, &Analysis::statusChanged, &loop,
					[&loop](Analysis* analysis) {
						if (analysis->isFinished())
							loop.quit();
					});
			});

		Json::Value response = JaspRpcDispatcher::successResult();
		_rpcWriteIdentity(response, a);
		_rpcWriteStatus(response, a);
		_rpcWriteOptionsDelta(response, a, false, !useDelta);
		if (a->isFinished())
			_rpcWriteFinishedResults(response, a, analysisId);

		AgentStateTracker::notifyAnalysisObserved(a->id());
		return response;
	});

	disp->registerMethodByName("get_analyses_state", [](const Json::Value& params) -> Json::Value
	{
		bool includeOptions  = params.get("include_options",    true).asBool();
		bool metaDiff        = params.get("options_meta_diff",  true).asBool();
		bool includeResults  = params.get("include_results",   false).asBool();
		bool includeDesc     = params.get("include_descriptions", false).asBool();

		std::vector<int> ids;
		for (const auto & v : params["analysisIds"])
			ids.push_back(v.asInt());

		Json::Value response = JaspRpcDispatcher::successResult();

		if (auto * t = AgentStateTracker::tracker())
		{
			Json::Value snapshot = t->buildAnalysesSnapshot(
				ids, includeOptions, includeResults, includeDesc, metaDiff);
			for (const auto & key : snapshot.getMemberNames())
				response[key] = snapshot[key];

			// Clear dirty flags for the analyses the agent just observed
			for (int id : ids)
				AgentStateTracker::notifyAnalysisObserved(static_cast<size_t>(id));
		}

		return response;
	});

	disp->registerMethodByName("analysis_results", [](const Json::Value& params) -> Json::Value
	{
		int analysisId = params["analysisId"].asInt();

		Json::Value error;
		Analysis* a = _rpcResolveAnalysis(analysisId, error);
		if (!a) return error;

		bool wait      = params["wait"].asBool();
		int  timeoutMs = params["timeoutMs"].asInt();

		// Fast path: results already ready — return immediately
		if (a->isFinished())
		{
			Json::Value response = JaspRpcDispatcher::successResult();
			_rpcWriteIdentity(response, a);
			_rpcWriteStatus(response, a);
			_rpcWriteFinishedResults(response, a, analysisId);
			AgentStateTracker::notifyAnalysisObserved(a->id());
			return response;
		}

		// Not waiting: return immediately with running status, no stale results
		if (!wait)
		{
			Json::Value response = JaspRpcDispatcher::successResult();
			_rpcWriteIdentity(response, a);
			response["status"] = "running";
			AgentStateTracker::notifyAnalysisObserved(a->id());
			return response;
		}

		// Collect all form errors (control-level + form-level) and return
		// immediately if any exist — results will never arrive.
		if (a->form())
		{
			std::string formErrorMsg;
			if (a->form()->hasError())
				formErrorMsg = fq(a->form()->getError(true));
			QString formErrors = a->form()->errors();
			if (!formErrors.isEmpty())
			{
				if (!formErrorMsg.empty()) formErrorMsg += ", ";
				formErrorMsg += fq(formErrors);
			}
			if (!formErrorMsg.empty())
				return JaspRpcDispatcher::errorResult(
					"Analysis has form validation errors: " + formErrorMsg);
		}

		JaspRpcDispatcher::waitAndProcessEvents(timeoutMs,
			[&](QEventLoop& loop, QTimer&) {
				QObject::connect(a, &Analysis::statusChanged, &loop,
					[&loop](Analysis* analysis) {
						if (analysis->isFinished())
							loop.quit();
					});
			});

		// Build response — may be finished or still running if timeout fired
		Json::Value response = JaspRpcDispatcher::successResult();
		_rpcWriteIdentity(response, a);
		_rpcWriteStatus(response, a);
		if (a->isFinished())
			_rpcWriteFinishedResults(response, a, analysisId);

		AgentStateTracker::notifyAnalysisObserved(a->id());
		return response;
	});

		// (analysis_setResults commented out — use analysis_composeResults instead)
		/*
		disp->registerMethodByName("analysis_setResults", [](const Json::Value& params) -> Json::Value
		{
			int analysisId = params["analysisId"].asInt();
			Analysis* a = Analyses::analyses()->get(static_cast<size_t>(analysisId));
			if (!a)
				return JaspRpcDispatcher::errorResult(
					"Analysis not found: " + std::to_string(analysisId));

			// 'results' is a JSON-encoded string — parse on our side
			Json::Reader reader;
			Json::Value results;
			if (!reader.parse(params["results"].asString(), results))
				return JaspRpcDispatcher::errorResult(
					"Failed to parse results JSON: " + reader.getFormattedErrorMessages());

			// Map the requested status string to Analysis::Status.
			// Default is Complete; also accept fatalError.
			Analysis::Status status = Analysis::Complete;
			if (params.isMember("status") && params["status"].asString() == "fatalError")
				status = Analysis::FatalError;

			a->setResults(results, status);

			Json::Value response = JaspRpcDispatcher::successResult();
			response["analysisId"] = analysisId;
			response["module"]     = a->module();
			response["analysis"]   = a->name();
			return response;
		});
		*/

	disp->registerMethodByName("analysis_composeResults", [](const Json::Value& params) -> Json::Value
	{
		int analysisId = params["analysisId"].asInt();
		Analysis* a = Analyses::analyses()->get(static_cast<size_t>(analysisId));
		if (!a)
			return JaspRpcDispatcher::errorResult(
				"Analysis not found: " + std::to_string(analysisId));

		Json::Value error;
		Json::Value composed = Analyses::composeResultJSON(params["elements"], analysisId, error);
		if (composed.isNull())
			return error;

		Analysis::Status status = Analysis::Complete;
		if (params.isMember("status") && params["status"].asString() == "fatalError")
			status = Analysis::FatalError;

		a->setResults(composed, status);

		Json::Value response = JaspRpcDispatcher::successResult();
		response["analysisId"] = analysisId;
		response["module"]     = a->module();
		response["analysis"]   = a->name();
		return response;
	});

	disp->registerMethodByName("analysis_createAnnotation", [](const Json::Value& params) -> Json::Value
	{
		int analysisId = params["analysisId"].asInt();
		Analysis* original = Analyses::analyses()->get(static_cast<size_t>(analysisId));
		if (!original)
			return JaspRpcDispatcher::errorResult(
				"Analysis not found: " + std::to_string(analysisId));

		Analysis* dup = Analyses::analyses()->duplicateAnalysis(analysisId, true);
		if (!dup)
			return JaspRpcDispatcher::errorResult("Failed to duplicate analysis");

		// Generate annotation title: "Annotation [N ]of <Original Title>"
		std::string origTitle = original->title();
		std::string baseTitle = "Annotation of " + origTitle;
		int annNum = 1;
		Analyses* ans = Analyses::analyses();
		for (size_t i = 0; i < ans->count(); i++)
		{
			Analysis* a = ans->operator[](i);
			if (a && a->id() != dup->id())
			{
				std::string t = a->title();
				if (t == baseTitle) annNum = std::max(annNum, 2);
				else if (t.find("Annotation ") == 0 && t.find(" of " + origTitle) != std::string::npos)
				{
					// Parse "Annotation N of ..."
					size_t end = t.find(" of ");
					std::string numStr = t.substr(11, end - 11); // "Annotation ".length() == 11
					try { int n = std::stoi(numStr); annNum = std::max(annNum, n + 1); } catch(...) {}
				}
			}
		}
		dup->setTitle(annNum > 1 ? "Annotation " + std::to_string(annNum) + " of " + origTitle : baseTitle);

		// Disable the form on the duplicate
		dup->setIsAnnotated(true);

		// Compose results into the duplicate (if elements provided; otherwise keep original's results)
		Json::Value elementList = params.isMember("elements") ? params["elements"] : Json::Value(Json::arrayValue);
		Json::Value composed;

		if (elementList.size() > 0)
		{
			Json::Value error;
			composed = Analyses::composeResultJSON(elementList, static_cast<int>(dup->id()), error);
			if (composed.isNull())
				return error;
		}

		Analysis::Status status = Analysis::Complete;
		if (params.isMember("status") && params["status"].asString() == "fatalError")
			status = Analysis::FatalError;

		if (elementList.size() > 0)
			dup->setResults(composed, status);
		else if (status == Analysis::FatalError)
			dup->setResults(dup->results(), status); // apply status without changing content

		Json::Value response = JaspRpcDispatcher::successResult();
		_rpcWriteIdentity(response, dup);
		_rpcWriteStatus(response, dup);
		return response;
	});

	disp->registerMethodByName("analysis_context", [](const Json::Value& params) -> Json::Value
	{
		QString module   = QString::fromStdString(params["module"].asString());
		QString analysis = QString::fromStdString(params["analysis"].asString());

		auto* dm = DynamicModules::dynMods();
		if (!dm)
			return JaspRpcDispatcher::errorResult("DynamicModules not available");

		auto* mod = dm->dynamicModule(fq(module));
		if (!mod)
			return JaspRpcDispatcher::errorResult("Module not found: " + fq(module));

		Modules::AnalysisEntry* entry = nullptr;
		for (auto* e : mod->menu())
			if (e->isAnalysis() && e->function() == fq(analysis))
			{
				entry = e;
				break;
			}

		if (!entry)
			return JaspRpcDispatcher::errorResult("Analysis not found: " + fq(module) + "::" + fq(analysis));

		// Read help file if available (<moduleInstFolder>/help/<functionName>.md)
		Json::Value help("");
		QString helpPath = mod->helpFolderPath() + QString::fromStdString(fq(analysis)) + ".md";
		QFile helpFile(helpPath);
		if (helpFile.open(QIODevice::ReadOnly | QIODevice::Text))
		{
			help = QString::fromUtf8(helpFile.readAll()).toStdString();
			helpFile.close();
		}

		Json::Value response = JaspRpcDispatcher::successResult();
		response["module"]   = module.toStdString();
		response["analysis"] = analysis.toStdString();
		response["help"]     = help;
		return response;
	});

	disp->registerMethodByName("modules_list", [](const Json::Value&) -> Json::Value
	{
		auto* dm = DynamicModules::dynMods();
		Json::Value modules(Json::arrayValue);

		for (const auto& modName : dm->moduleNames())
		{
			if (auto* mod = dm->dynamicModule(modName))
			{
				Json::Value m;
				m["name"]  = modName;
				m["title"] = mod->title();

				Json::Value analyses(Json::arrayValue);
				for (const auto* entry : mod->menu())
				{
					if (!entry->isAnalysis() || entry->isSeparator())
						continue;

					Json::Value a;
					a["name"]  = entry->function();
					a["title"] = entry->title();
					analyses.append(a);
				}
				m["analyses"] = analyses;
				modules.append(m);
			}
		}

		Json::Value result;
		result["modules"] = modules;
		return result;
	});

	disp->registerMethodByName("analyses_list", [](const Json::Value&) -> Json::Value
	{
		auto* ans = Analyses::analyses();
		Json::Value analysesArr(Json::arrayValue);

		ans->applyToAll([&analysesArr](Analysis* a)
		{
			if (a->isReport()) return; // skip reports — they have no module to reference

			Json::Value entry;
			entry["id"]       = static_cast<int>(a->id());
			entry["module"]   = a->module();
			entry["analysis"] = a->name();
			entry["title"]    = a->title();
			analysesArr.append(entry);
		});

		// Resolve the active analysis: currentAnalysisIndex is a row index, not an ID.
		int activeId = -1;
		int curIdx   = ans->currentAnalysisIndex();
		if (curIdx >= 0 && curIdx < ans->count())
		{
			Analysis* active = (*ans)[static_cast<size_t>(curIdx)];
			if (active)
				activeId = static_cast<int>(active->id());
		}

		Json::Value result;
		result["analyses"]         = analysesArr;
		result["activeAnalysisId"] = activeId;
		return result;
	});

	disp->registerMethod("write_report", [](const Json::Value& params) -> Json::Value
	{
		Analyses* ans = Analyses::analyses();

		// --- Resolve report: update existing or create new ---
		Analysis* report = nullptr;
		bool isNew = !params.isMember("reportId");

		if (!isNew)
		{
			int reportId = params["reportId"].asInt();
			report = ans->get(static_cast<size_t>(reportId));
			if (!report || !report->isReport())
				return JaspRpcDispatcher::errorResult(
					"Report not found or is not a report: " + std::to_string(reportId));
		}

		// --- Title with duplication detection (only for new reports) ---
		std::string userTitle = params.get("title", "").asString();
		std::string title;

		if (isNew)
		{
			std::string prefix = "Report" + (userTitle.empty() ? "" : ": " + userTitle);
			std::string baseTitle = prefix;
			int reportNum = 1;
			for (size_t i = 0; i < ans->count(); i++)
			{
				Analysis* a = ans->operator[](i);
				if (a)
				{
					std::string t = a->title();
					if (t == baseTitle)
						reportNum = std::max(reportNum, 2);
					else if (t.rfind(prefix + " ", 0) == 0)
					{
						std::string suffix = t.substr(prefix.size() + 1);
						try { int n = std::stoi(suffix); reportNum = std::max(reportNum, n + 1); }
						catch(...) {}
					}
				}
			}
			title = reportNum > 1 ? prefix + " " + std::to_string(reportNum) : prefix;

			report = ans->createReport(title);
			if (!report)
				return JaspRpcDispatcher::errorResult("Failed to create report");
		}
		else if (!userTitle.empty())
		{
			// Update title on existing report
			title = "Report: " + userTitle;
			report->setTitle(title);
		}
		else
		{
			title = report->title();
		}

		// --- Compose results (cross-analysis via sourceAnalysisId) ---
		Json::Value elementList = params.isMember("elements") ? params["elements"] : Json::Value(Json::arrayValue);

		if (elementList.size() == 0)
		{
			Json::Value defaultEl(Json::objectValue);
			defaultEl["md_text"] = "# Hello World!\\n\\nLorem ipsum dolor sit amet, consectetur adipiscing elit. "
				"Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, "
				"quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\\n\\n"
				"## Section 2\\n\\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore "
				"eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui "
				"officia deserunt mollit anim id est laborum.";
			elementList.append(defaultEl);
		}

		Json::Value error;
		Json::Value composed = Analyses::composeResultJSON(elementList, -1, error);
		if (composed.isNull())
			return error;

		Analysis::Status analysisStatus = Analysis::Complete;
		if (params.isMember("status") && params["status"].asString() == "fatalError")
			analysisStatus = Analysis::FatalError;

		report->setResults(composed, analysisStatus);

		Json::Value response = JaspRpcDispatcher::successResult();
		response["reportId"] = static_cast<int>(report->id());
		response["title"]    = report->title();
		return response;
	});
}
