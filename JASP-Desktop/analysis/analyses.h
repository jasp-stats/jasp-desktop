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

#include <QString>
#include <QMap>
#include <QAbstractListModel>

class Analyses : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(size_t	count					READ count													NOTIFY countChanged)
	Q_PROPERTY(int		currentAnalysisIndex	READ currentAnalysisIndex	WRITE setCurrentAnalysisIndex	NOTIFY currentAnalysisIndexChanged)

	friend class EngineSync;
	friend class boost::iterator_core_access;

	typedef QMap<int, Analysis *> ById;
public:

	enum myRoles {	formPathRole = Qt::UserRole + 1,
					titleRole,
					nameRole };

				Analyses(QObject * parent) : QAbstractListModel(parent) {}

	Analysis*	createFromJaspFileEntry(Json::Value analysisData, DynamicModules * dynamicModules);
	Analysis*	create(const QString &module, const QString &name, size_t id, const Version &version, Json::Value *options = nullptr, Analysis::Status status = Analysis::Empty);
	Analysis*	create(Modules::AnalysisEntry * analysisEntry, size_t id, Analysis::Status status = Analysis::Empty);

	Analysis*	create(const QString &module, const QString &name)	{ return create(module, name, _nextId++, AppInfo::version);		}
	Analysis*	create(Modules::AnalysisEntry * analysisEntry)		{ return create(analysisEntry, _nextId++);						}

	Analysis*	get(size_t id) const								{ return _analysisMap.count(id) > 0 ? _analysisMap.at(id) : nullptr;	}
	void		clear();

	void		setAnalysesUserData(Json::Value userData);
	void		refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns,	 std::vector<std::string> &missingColumns,	 std::map<std::string, std::string> &changeNameColumns,	 std::vector<std::string> &oldColumnNames);

	///Applies function to some or all analyses, if applyThis returns false it stops processing.
	void		applyToSome(std::function<bool(Analysis *analysis)> applyThis);

	///Applies function to all analyses.
	void		applyToAll(std::function<void(Analysis *analysis)> applyThis);

	size_t		count() const	{ assert(_analysisMap.size() == _orderedIds.size()); return _analysisMap.size(); }

	Json::Value asJson() const;

	void		selectAnalysis(Analysis * analysis);

//AbstractListModel functions
public:
	int						rowCount(const QModelIndex & = QModelIndex())				const override	{	return int(count()); }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>	roleNames()													const override;
	int						currentAnalysisIndex()										const			{	return _currentAnalysisIndex;	}

public slots:
	void removeAnalysisById(size_t id);
	void removeAnalysis(Analysis *analysis);
	void refreshAllAnalyses();
	void refreshAnalysesUsingColumn(QString col);
	void analysisClickedHandler(QString, QString);
	void setCurrentAnalysisIndex(int currentAnalysisIndex);


signals:
	void analysisAdded(					Analysis *source);
	void analysisEditImage(				Analysis *source);
	void analysisSaveImage(				Analysis *source);
	void analysisToRefresh(				Analysis *source);
	void analysisImageSaved(			Analysis *source);
	void analysisInitialised(			Analysis *source);
	void analysisImageEdited(			Analysis *source);
	void analysisResultsChanged(		Analysis *source);
	void analysisOptionsChanged(		Analysis *source);

	ComputedColumn *	requestComputedColumnCreation(std::string columnName, Analysis *source);
	void				requestComputedColumnDestruction(std::string columnName);

	void countChanged();
	void currentAnalysisIndexChanged(int currentAnalysisIndex);

private:
	void bindAnalysisHandler(Analysis* analysis);
	void storeAnalysis(Analysis* analysis, size_t id);

	void analysisOptionsChangedHandler(	Analysis *analysis)							{ analysisOptionsChanged(analysis); }
	void analysisImageSavedHandler(		Analysis *analysis)							{ analysisImageSaved(analysis); }
	void analysisImageEditedHandler(	Analysis *analysis)							{ analysisImageEdited(analysis); }
	void analysisResultsChangedHandler(	Analysis *analysis)							{ analysisResultsChanged(analysis); }
	void analysisToRefreshHandler(		Analysis *analysis);
	void analysisSaveImageHandler(		Analysis *analysis, Json::Value &options);
	void analysisEditImageHandler(		Analysis *analysis, Json::Value &options);

private:
	 std::map<size_t, Analysis*>	_analysisMap;
	 std::vector<size_t>			_orderedIds;

	 size_t _nextId					= 0;
	 int	_currentAnalysisIndex	= -1;
};

#endif // ANALYSES_H
