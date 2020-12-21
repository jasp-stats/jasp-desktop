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
#include "data/datasetpackage.h"
#include "modules/upgrader/upgrader.h"

#include <QString>
#include <QMap>
#include <QAbstractListModel>
#include <QFileSystemWatcher>
#include <sstream>

class RibbonModel;

class Analyses : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(int		count					READ count													NOTIFY countChanged)
	Q_PROPERTY(int		currentAnalysisIndex	READ currentAnalysisIndex	WRITE setCurrentAnalysisIndex	NOTIFY currentAnalysisIndexChanged)
	Q_PROPERTY(double	currentFormHeight		READ currentFormHeight		WRITE setCurrentFormHeight		NOTIFY currentFormHeightChanged)
	Q_PROPERTY(double	currentFormPrevH		READ currentFormPrevH		WRITE setCurrentFormPrevH		NOTIFY currentFormPrevHChanged)
	Q_PROPERTY(bool		visible					READ visible				WRITE setVisible				NOTIFY visibleChanged)
	Q_PROPERTY(bool		moving					READ moving					WRITE setMoving					NOTIFY movingChanged)


	friend class EngineSync;
	friend class boost::iterator_core_access;

	typedef QMap<int, Analysis *> ById;
public:

	enum myRoles {	formPathRole = Qt::UserRole + 1,
					analysisRole,
					titleRole,
					nameRole,
					idRole};

						Analyses();
						~Analyses() { _singleton = nullptr; }
	static Analyses *	analyses() { return _singleton; }

	Analysis	*	createFromJaspFileEntry(Json::Value analysisData, RibbonModel* ribbonModel);

	Analysis	*	create(Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status = Analysis::Initializing, bool notifyAll = true, std::string title = "", std::string moduleVersion = "", Json::Value *options = nullptr);
	Analysis	*	create(Modules::AnalysisEntry * analysisEntry)													{ return create(analysisEntry, _nextId++);						}

	Analysis	*	operator[](size_t index)	{ return _analysisMap[_orderedIds[index]]; }
	Analysis	*	get(size_t id) const		{ return _analysisMap.count(id) > 0 ? _analysisMap.at(id) : nullptr;	}

	void			clear();
	void			reload(Analysis* analysis, bool logProblem);

	bool			allFresh() const;
	void			setAnalysesUserData(Json::Value userData);
	void			loadAnalysesFromDatasetPackage(bool & errorFound, std::stringstream & errorMsg, RibbonModel * ribbonModel);

	///Applies function to some or all analyses, if applyThis returns false it stops processing.
	void		applyToSome(std::function<bool(Analysis *analysis)> applyThis);

	///Applies function to all analyses.
	void		applyToAll(std::function<void(Analysis *analysis)> applyThis);
	void		applyToAll(std::function<void(Analysis *analysis)> applyThis) const;

	int			count() const	{ assert(_analysisMap.size() == _orderedIds.size()); return _analysisMap.size(); }

	Json::Value asJson() const;

	void		selectAnalysis(Analysis * analysis);
	
	int						rowCount(const QModelIndex & = QModelIndex())				const override	{ return int(count()); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>	roleNames()													const override;
	int						currentAnalysisIndex()										const			{ return _currentAnalysisIndex;	}
	double					currentFormHeight()											const			{ return _currentFormHeight;	}
	bool					visible()													const			{ return _visible;				}
	bool					moving()													const			{ return _moving;				}
	double					currentFormPrevH()											const			{ return _currentFormPrevH;		}
	Json::Value				resultsMeta()												const			{ return _resultsMeta;			}
	Json::Value				allUserData()												const			{ return _allUserData;			}
	Analysis*				getAnalysisBeforeMoving(size_t index);

public slots:
	void removeAnalysisById(size_t id);
	void removeAnalysis(Analysis *analysis);
	void refreshAllAnalyses();
	void refreshAllPlots(std::set<Analysis*> exceptThese = {});
	void analysisClickedHandler(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module);
	void setCurrentAnalysisIndex(int currentAnalysisIndex);
	void analysisIdSelectedInResults(int id);
	void analysesUnselectedInResults();
	void selectAnalysisAtRow(int row);
	void unselectAnalysis();
	void rCodeReturned(QString result, int requestId);
	void setCurrentFormHeight(double currentFormHeight);
	void setVisible(bool visible);
	void setMoving(bool moving);
	void removeAnalysesOfDynamicModule(Modules::DynamicModule * module);
	void refreshAnalysesOfDynamicModule(Modules::DynamicModule * module);
	void replaceAnalysesOfDynamicModule(Modules::DynamicModule * oldModule, Modules::DynamicModule * newModule);
	void rescanAnalysisEntriesOfDynamicModule(Modules::DynamicModule * module);
	void setChangedAnalysisTitle();
	void analysisTitleChangedInResults(int id, QString title);
	void setCurrentFormPrevH(double currentFormPrevH);
	void move(int fromIndex, int toIndex);
	void duplicateAnalysis(size_t id);
	void showDependenciesInAnalysis(size_t analysis_id, QString optionName);
	void analysisTitleChangedHandler(std::string moduleName, std::string oldTitle, std::string newTitle);
	void languageChangedHandler();
	void resultsMetaChanged(QString json);
	void allUserDataChanged(QString json);
	void moveAnalysesResults(Analysis* fromAnalysis, int index);

signals:
	void analysesUnselected();
	void unselectAnalysisInResults();
	void countChanged();
	void analysisAdded(					Analysis *	source);
	void analysisRemoved(				Analysis *	source);
	void analysisImageSaved(			Analysis *	source);
	void analysisImageEdited(			Analysis *	source);
	void analysisResultsChanged(		Analysis *	source);
	void analysisTitleChanged(			Analysis *  source);
	void analysisOverwriteUserdata(		Analysis *	source);
	void analysisStatusChanged(			Analysis *	source);
	void sendRScript(					QString		script, int requestID, bool whiteListedVersion);
	void analysisSelectedIndexResults(	int			row);
	void showAnalysisInResults(			int			id);
	void currentAnalysisIndexChanged(	int			currentAnalysisIndex);
	void currentFormHeightChanged(		double		currentFormHeight);
	void visibleChanged(				bool		visible);
	void movingChanged(					bool		moving);
	void emptyQMLCache();
	void dataSetChanged();
	void somethingModified();
    void analysesExportResults();
	bool developerMode();
	void setResultsMeta(QString json);
	void moveAnalyses(quint64 fromId, quint64 toId);

	ComputedColumn *	requestComputedColumnCreation(QString columnName, Analysis *source);
	void				requestColumnCreation(QString columnName, Analysis *source, int columnType);
	void				requestComputedColumnDestruction(QString columnName);

	void currentFormPrevHChanged(double currentFormPrevH);

private slots:
	void sendRScriptHandler(Analysis* analysis, QString script, QString controlName, bool whiteListedVersion);

private:
	void bindAnalysisHandler(Analysis* analysis);
	void storeAnalysis(Analysis* analysis, size_t id, bool notifyAll);	
	void _makeBackwardCompatible(RibbonModel* ribbonModel, Version& version, Json::Value& analysisData);
	void _analysisQMLFileChanged(Analysis* analysis);


private:
	static Analyses				*	_singleton;

	Json::Value						_resultsMeta, //Stored Notes and custom title
									_allUserData; //Notes and stuff?

	std::map<size_t, Analysis*>		_analysisMap;
	std::vector<size_t>				_orderedIds;
	std::vector<size_t>				_orderedIdsBeforeMoving;
	QFileSystemWatcher				_QMLFileWatcher;

	size_t							_nextId					= 0;
	int								_currentAnalysisIndex	= -1;
	double							_currentFormHeight		= 0,
									_currentFormPrevH		= -1;
	bool							_visible				= false;
	bool							_moving					= false;

	static int								_scriptRequestID;
	QMap<int, QPair<Analysis*, QString> >	_scriptIDMap;

};

#endif // ANALYSES_H
