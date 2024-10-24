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
#include "utilities/settings.h"
#include "modules/ribbonmodel.h"
#include "analysisform.h"
#include "knownissues.h"
#include "timers.h"
#include <QTimer>
#include <QFile>
#include "log.h"

using namespace std;
using Modules::Upgrader;

Analyses * Analyses::_singleton = nullptr;

Analyses::Analyses()
	: QAbstractListModel(DataSetPackage::pkg())
{
	if(_singleton) throw std::runtime_error("Can only instantiate single copy of Analyses!");
	_singleton = this;

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

	Analysis				*	analysis		= nullptr;
	Modules::UpgradeMsgs		msgs;
	bool						wasUpgraded		= Upgrader::upgrader()->upgradeAnalysisData(analysisData, msgs);
	Json::Value				&	optionsJson		= analysisData["options"];
	Modules::AnalysisEntry	*	analysisEntry	= nullptr;

	if(analysisData.get("dynamicModule", Json::nullValue).isNull()) //This used to be the code to load our "standard modules", seeing as how those are gone and the've become dynamic as well it should figure out somehow which one is meant
	{
		Log::log() << "It is a builtin analysis, " << std::flush;
		
		Json::Value	&	versionJson		= analysisData["version"];
		Version			version			= Version(versionJson.isNull() ? "0.0.0" : versionJson.asString()); //0.0.0 as module version to be sure we show the "made with old version". If the version was given we should make sure all the modules have a version > 0.14.2

		QString			name			= tq(analysisData["name"].asString()),
						module			= analysisData["module"].asString() != "" ? tq(analysisData["module"].asString()) : "Common",
						title			= tq(analysisData.get("title", "").asString());
						analysisEntry	= ribbonModel->getAnalysis(module.toStdString(), name.toStdString());
		QString			qml				= analysisEntry ? tq(analysisEntry->qml()) : name + ".qml";
		
		Log::log() << " titled: '" << title << "'" << std::endl;

		if(!analysisEntry)
			throw Modules::ModuleException(fq(module), "Problem loading analysis \"" + fq(name) + "\" from module \"" + fq(module) + "\". This module cannot be found in your JASP or the analysis cannot be found in it, this is likely a bug as we still ship all modules in the installer.");

		if(title == "")
			title = tq(analysisEntry->title());
		
		analysis = create(analysisData, analysisEntry, id, status, false, fq(title), version.asString(3), &optionsJson); 
	}
	else
	{
		std::string title			= analysisData.get("title", "").asString();
		
		Log::log() << "It is a dynmod analysis with title: '" << title << "'" << std::endl;
		
		analysisEntry		= Modules::DynamicModules::dynMods()->retrieveCorrespondingAnalysisEntry(analysisData["dynamicModule"]);
		analysis			= create(analysisData, analysisEntry, id, status, false, title, analysisData["dynamicModule"]["moduleVersion"].asString(), &optionsJson);
		
		if(msgs.count(Modules::analysisLog))
		{
			QStringList msgAna = tq(msgs[Modules::analysisLog]);
			analysis->setErrorInResults(fq(msgAna.join("\n")));
		}
	}

	if(wasUpgraded)
		analysis->setUpgradeMsgs(msgs);

	return analysis;
}

Analysis* Analyses::create(const Json::Value & analysisData, Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status, bool notifyAll, std::string title, std::string moduleVersion, Json::Value *options)
{
	Analysis *analysis = new Analysis(id, analysisEntry, title, moduleVersion, options);

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
					a->createForm();
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
		int				corruptAnalyses = 0;
		stringstream	corruptionStrings;

		Json::Value analysesData = DataSetPackage::pkg()->analysesData();
		if (analysesData.isNull())
		{
			errorFound = true;
			errorMsg << "An error has been detected and analyses could not be loaded.";
		}
		else
		{
			Json::Value analysesDataList = analysesData;
			if (!analysesData.isArray())
			{
				analysesDataList = analysesData.get("analyses", Json::arrayValue);
				Json::Value meta = analysesData.get("meta",		Json::nullValue);

				if (!meta.isNull())
				{
					QString results = tq(analysesData["meta"].toStyledString());
					resultsMetaChanged(results);
					emit setResultsMeta(results);
				}
			}

			JASPTIMER_START(Analyses::loadAnalysesFromDatasetPackage f-o-r analysisData in analysesDataList);

			Log::log() << "Loading analyses from jasp-file, entering loop." << std::endl;
			
			//There is no point trying to show progress here because qml is not updated while this function runs...
			for (Json::Value & analysisData : analysesDataList)
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

			JASPTIMER_STOP(Analyses::loadAnalysesFromDatasetPackage for analysisData : analysesDataList);
		}

		if (corruptAnalyses == 1)			errorMsg << "An error was detected in an analysis. This analysis has been removed for the following reason:\n" << corruptionStrings.str();
		else if (corruptAnalyses > 1)		errorMsg << "Errors were detected in " << corruptAnalyses << " analyses. These analyses have been removed for the following reasons:\n" << corruptionStrings.str();
		else								Log::log() << "Loading analyses seems to have worked out fine." << std::endl;
	}
	

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
	Modules::DynamicModule * dynamicModule = Modules::DynamicModules::dynMods()->dynamicModule(module.toStdString());

	if (dynamicModule)	return create(dynamicModule->retrieveCorrespondingAnalysisEntry(fq(analysis)));
	else				return nullptr;

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
	Settings::setValue(Settings::SHOW_RSYNTAX_IN_RESULTS, show);

	applyToAll([&](Analysis * a)
	{
		a->setRSyntaxTextInResult();
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

void Analyses::duplicateAnalysis(size_t id)
{
	if(!get(id)) return;

	Analysis	* original = get(id),
				* analysis = new Analysis(++_nextId, original);

	storeAnalysis(analysis, analysis->id(), true);
	bindAnalysisHandler(analysis);
	analysis->emitDuplicationSignals();

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
