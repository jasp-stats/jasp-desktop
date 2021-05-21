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

#ifndef ANALYSISMENUMODEL_H
#define ANALYSISMENUMODEL_H

#include <QAbstractListModel>
#include <QStringList>

#include "modules/analysisentry.h"
#include "modules/dynamicmodule.h"

class RibbonButton;

//This class should not hold it's own data but simply be an interface for the description of a RibbonButton/Dynamic Module
class MenuModel : public QAbstractListModel
{
	Q_OBJECT

public:
	enum {
		DisplayRole,
		AnalysisFunctionRole,
		MenuImageSourceRole,
		IsSeparatorRole,
		isGroupTitleRole,
		IsEnabledRole
	};

	MenuModel(RibbonButton * parent, Modules::DynamicModule		* module);
	MenuModel(RibbonButton * parent, Modules::AnalysisEntries	* entries = new Modules::AnalysisEntries()); //The default new entries is to make sure that analysisEntries() has something to return for special buttons without their own entries

	~MenuModel()
	{
		if(_entries) delete _entries;
		_entries = nullptr;
	}

	int										rowCount(const QModelIndex &parent = QModelIndex())			const override	{	return analysisEntries().size();	}
	QVariant								data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	QHash<int, QByteArray>					roleNames()													const override;


	const	Modules::AnalysisEntries &		analysisEntries() const;

	Modules::AnalysisEntry*					getAnalysisEntry(const std::string& func);

	Q_INVOKABLE QString						getFirstAnalysisFunction()													{	return getAnalysisFunction(0);	}
	Q_INVOKABLE QString						getFirstAnalysisTitle()														{	return getAnalysisTitle(0);		}
	Q_INVOKABLE QString						getFirstAnalysisQML()														{	return getAnalysisQML(0);		}
	Q_INVOKABLE QString						getAnalysisFunction(int index)								const			{	return QString::fromStdString(analysisEntries().at(index)->function());	}
	Q_INVOKABLE QString						getAnalysisTitle(	int index)								const			{	return QString::fromStdString(analysisEntries().at(index)->title());	}
	Q_INVOKABLE QString						getAnalysisQML(		int index)								const			{	return QString::fromStdString(analysisEntries().at(index)->qml());	}
	Q_INVOKABLE bool						isAnalysisEnabled(	int index);
	void									setDynamicModule(Modules::DynamicModule * module)							{ beginResetModel(); _module = module; endResetModel(); }

	Q_INVOKABLE bool						hasIcons()													const			{	return _hasIcons; }

private:
	RibbonButton				*	_ribbonButton	= nullptr;
	Modules::DynamicModule		*	_module			= nullptr;
	Modules::AnalysisEntries	*	_entries		= nullptr;		//For special buttons
	bool							_hasIcons		= false;
};

#endif
