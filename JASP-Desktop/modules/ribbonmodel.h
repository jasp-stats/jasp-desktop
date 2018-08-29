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


#ifndef RIBBONMODEL_H
#define RIBBONMODEL_H

#include <QAbstractListModel>
#include <QStringList>
#include <QDebug>

#include "modules/ribbonbuttonmodel.h"


class RibbonModel : public QAbstractListModel
{
	Q_OBJECT

public:
	enum {
		ClusterRole = Qt::UserRole,
		DisplayRole
	};

	RibbonModel(QObject *parent) : QAbstractListModel(parent) {}

	int								rowCount(const QModelIndex &parent = QModelIndex())			const override	{	return _moduleNames.size();	}
	QVariant						data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;
	virtual QHash<int, QByteArray>	roleNames()													const override;

	// custom functions
	void 						addRibbonName(std::string name)					{	_moduleNames.push_back(name);	}
	void 						addRibbonButtonModel(RibbonButtonModel* model)	{	_ribbonButtonModels.push_back(model);	}
	std::vector<std::string>	moduleNames()									{	return _moduleNames;	}

	bool						isModuleName(std::string);
	RibbonButtonModel*			ribbonButtonModel(std::string);

private:
	// TODO: Replace the two lists below with QMap<QString, RibbonButtonModel*>
	std::vector<std::string>		_moduleNames;
	std::vector<RibbonButtonModel*>	_ribbonButtonModels;
};


#endif  // RIBBONMODEL_H
