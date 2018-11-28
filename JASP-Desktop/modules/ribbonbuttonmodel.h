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

#ifndef RIBBONBUTTONMODEL_H
#define RIBBONBUTTONMODEL_H

#include <QAbstractListModel>
#include <QStringList>

#include "modules/ribbonentry.h"
#include "modules/analysismenumodel.h"


class RibbonButtonModel : public QAbstractListModel
{
	Q_OBJECT

public:
	enum {
		AnalysisMenuRole = Qt::UserRole,
		DisplayRole,
		IconSourceRole
	};

	RibbonButtonModel(QObject *parent) : QAbstractListModel(parent) {}

	int								rowCount(const QModelIndex &parent = QModelIndex())			const override	{	return _ribbonEntries.size();	}
	QVariant						data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual	QHash<int, QByteArray>	roleNames()													const override;
	// Utility functions
	void							setRequiresDataset(bool requiresDataset)									{	_requiresDataset = requiresDataset;	}
	void							setDynamic(bool isDynamicModule)											{	_isDynamicModule = isDynamicModule;	}
	Q_INVOKABLE bool				requiresDataset()															{	return _requiresDataset;	}
	Q_INVOKABLE bool				isDynamic()																	{	return _isDynamicModule;	}
	void							setRibbonEntries(std::vector<Modules::RibbonEntry*>);

private:
	std::vector<Modules::RibbonEntry*>	_ribbonEntries;
	std::vector<AnalysisMenuModel*> 	_analysisMenuModels;

	bool								_requiresDataset = true;
	bool								_isDynamicModule = true;
};


#endif
