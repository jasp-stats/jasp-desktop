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
#include "dataset.h"

#include <QString>
#include <QMap>
#include <QAbstractListModel>
#include <QFileSystemWatcher>

class RibbonModel;

class Analyses : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(int		count					READ count													NOTIFY countChanged)
	Q_PROPERTY(int		currentAnalysisIndex	READ currentAnalysisIndex	WRITE setCurrentAnalysisIndex	NOTIFY currentAnalysisIndexChanged)
	Q_PROPERTY(double	currentFormHeight		READ currentFormHeight		WRITE setCurrentFormHeight		NOTIFY currentFormHeightChanged)
	Q_PROPERTY(bool		visible					READ visible				WRITE setVisible				NOTIFY visibleChanged)


	friend class EngineSync;
	friend class boost::iterator_core_access;

	typedef QMap<int, Analysis *> ById;
public:

	enum myRoles {	formPathRole = Qt::UserRole + 1,
					analysisRole,
					titleRole,
					nameRole,
					idRole};

				Analyses(QObject * parent, DynamicModules * dynamicModules) : QAbstractListModel(parent), _dynamicModules(dynamicModules) {}

	Analysis*	createFromJaspFileEntry(Json::Value analysisData, RibbonModel* ribbonModel);
	Analysis*	create(const QString &module, const QString &name, const QString &title, size_t id, const Version &version, Json::Value *options = nullptr, Analysis::Status status = Analysis::Initializing, bool notifyAll = true);
	Analysis*	create(Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status = Analysis::Initializing, bool notifyAll = true, std::string title = "");

	Analysis*	create(const QString &module, const QString &name, const QString &title)	{ return create(module, name, title, _nextId++, AppInfo::version);		}
	Analysis*	create(Modules::AnalysisEntry * analysisEntry)								{ return create(analysisEntry, _nextId++);						}

	Analysis*	get(size_t id) const								{ return _analysisMap.count(id) > 0 ? _analysisMap.at(id) : nullptr;	}
	void		clear();
	void		reload(Analysis* analysis);

	void		setAnalysesUserData(Json::Value userData);
	void		refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns,	 std::vector<std::string> &missingColumns,	 std::map<std::string, std::string> &changeNameColumns,	 std::vector<std::string> &oldColumnNames);

	///Applies function to some or all analyses, if applyThis returns false it stops processing.
	void		applyToSome(std::function<bool(Analysis *analysis)> applyThis);

	///Applies function to all analyses.
	void		applyToAll(std::function<void(Analysis *analysis)> applyThis);

	int			count() const	{ assert(_analysisMap.size() == _orderedIds.size()); return _analysisMap.size(); }

	Json::Value asJson() const;

	void		selectAnalysis(Analysis * analysis);
	
	void		setDataSet(DataSet* dataSet);
	DataSet*	getDataSet() const				{ return _dataSet; }

	int						rowCount(const QModelIndex & = QModelIndex())				const override	{ return int(count()); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>	roleNames()													const override;
	int						currentAnalysisIndex()										const			{ return _currentAnalysisIndex;	}
	double					currentFormHeight()											const			{ return _currentFormHeight;	}
	bool					visible()													const			{ return _visible;				}

public slots:
	void removeAnalysisById(size_t id);
	void removeAnalysis(Analysis *analysis);
	void refreshAllAnalyses();
	void refreshAllPlots(std::set<Analysis*> exceptThese = {});
	void refreshAnalysesUsingColumn(QString col);
	void analysisClickedHandler(QString analysisFunction, QString analysisTitle, QString module);
	void setCurrentAnalysisIndex(int currentAnalysisIndex);
	void analysisIdSelectedInResults(int id);
	void analysesUnselectedInResults();
	void selectAnalysisAtRow(int row);
	void unselectAnalysis();
	void rCodeReturned(QString result, int requestId);
	void setCurrentFormHeight(double currentFormHeight);
	void setVisible(bool visible);
	void removeAnalysesOfDynamicModule(Modules::DynamicModule * module);
	void refreshAnalysesOfDynamicModule(Modules::DynamicModule * module);
	void rescanAnalysisEntriesOfDynamicModule(Modules::DynamicModule * module);
	void setChangedAnalysisTitle();
	void analysisTitleChangedInResults(int id, QString title);

signals:
	void analysesUnselected();
	void unselectAnalysisInResults();
	void countChanged();
	void analysisAdded(					Analysis *	source);
	void analysisRemoved(				Analysis *	source);
	void analysisEditImage(				Analysis *	source);
	void analysisSaveImage(				Analysis *	source);
	void analysisToRefresh(				Analysis *	source);
	void analysisImageSaved(			Analysis *	source);
	void analysisImageEdited(			Analysis *	source);
	void analysisRewriteImages(			Analysis *	source);
	void analysisResultsChanged(		Analysis *	source);
	void analysisTitleChanged(			Analysis *  source);
	void analysisOptionsChanged(		Analysis *	source);
	void sendRScript(					QString		script, int requestID);
	void analysisSelectedIndexResults(	int			row);
	void showAnalysisInResults(			int			id);
	void currentAnalysisIndexChanged(	int			currentAnalysisIndex);
	void currentFormHeightChanged(		double		currentFormHeight);
	void visibleChanged(				bool		visible);
	void emptyQMLCache();
	void dataSetChanged();
    void analysesExportResults();

	ComputedColumn *	requestComputedColumnCreation(QString columnName, Analysis *source);
	void				requestColumnCreation(QString columnName, Analysis *source, int columnType);
	void				requestComputedColumnDestruction(QString columnName);


private slots:
	void sendRScriptHandler(Analysis* analysis, QString script, QString controlName);

private:
	void bindAnalysisHandler(Analysis* analysis);
	void storeAnalysis(Analysis* analysis, size_t id, bool notifyAll);

private:
	 std::map<size_t, Analysis*>	_analysisMap;
	 std::vector<size_t>			_orderedIds;

	 size_t							_nextId					= 0;
	 int							_currentAnalysisIndex	= -1;
	 DataSet*						_dataSet				= nullptr;
	 DynamicModules*				_dynamicModules			= nullptr;
	 double							_currentFormHeight		= 0;
	 bool							_visible				= false;

	 static int								_scriptRequestID;
	 QMap<int, QPair<Analysis*, QString> >	_scriptIDMap;
	 
	 QFileSystemWatcher				_QMLFileWatcher;
	 
	 void							_makeBackwardCompatible(RibbonModel* ribbonModel, Version& version, Json::Value& analysisData);
	 void							_analysisQMLFileChanged(Analysis* analysis);
	 

};

#endif // ANALYSES_H
