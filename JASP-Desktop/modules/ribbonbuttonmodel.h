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
#include <QDir>

#include "modules/dynamicmodules.h"
#include "modules/analysismenumodel.h"



class RibbonButtonModel : public QAbstractListModel
{
	Q_OBJECT

public:
	enum {
		AnalysisMenuRole = Qt::UserRole,
		DisplayRole,
		IconSourceRole,
		EnabledRibbonRole
	};

	RibbonButtonModel(QObject *parent, Json::Value description);
	RibbonButtonModel(QObject *parent, Modules::DynamicModule * module);

	int								rowCount(const QModelIndex &parent = QModelIndex())			const override	{	return _ribbonEntries.size();	}
	QVariant						data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual	QHash<int, QByteArray>	roleNames()													const override;
	// Utility functions
	void							setRequiresDataset(bool requiresDataset)									{ _requiresDataset = requiresDataset;	}
	void							setDynamic(bool isDynamicModule)											{ _isDynamicModule = isDynamicModule;	}
	void							setTitle(std::string title)													{ _title = title;						}
	Q_INVOKABLE bool				requiresDataset()											const			{ return _requiresDataset;				}
	Q_INVOKABLE bool				isDynamic()													const			{ return _isDynamicModule;				}
	std::string						title()														const			{ return _title;						}
	void							setRibbonEntries(Modules::RibbonEntries ribbonEntries);

private:
	Modules::RibbonEntries			_ribbonEntries;
	AnalysisMenuModels				_analysisMenuModels;

	bool							_requiresDataset = true;
	bool							_isDynamicModule = true;
	std::string						_title = "";
};


#endif
