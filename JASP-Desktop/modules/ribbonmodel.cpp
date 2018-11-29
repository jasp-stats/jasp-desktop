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

#include <QDebug>
#include "ribbonmodel.h"

void RibbonModel::connectToDynamicModules(DynamicModules * dynamicModules)
{
	connect(dynamicModules, &DynamicModules::dynamicModuleAdded,	this, &RibbonModel::addDynamicRibbonButtonModel);
	connect(dynamicModules, &DynamicModules::dynamicModuleRemoved,	this, &RibbonModel::removeDynamicRibbonButtonModel);

	for(const std::string & modName : dynamicModules->moduleNames())
		addRibbonButtonModelFromDynamicModule((*dynamicModules)[modName]);
}


void RibbonModel::addRibbonButtonModelFromModulePath(QFileInfo modulePath)
{
	if(!modulePath.exists())
		return;

	QFile descriptionFile(modulePath.absolutePath() + "/description.json");
	if(!descriptionFile.exists())
		return;

	descriptionFile.open(QFile::ReadOnly);
	std::string	descriptionTxt(descriptionFile.readAll().toStdString());

	Json::Value descriptionJson;

	if(Json::Reader().parse(descriptionTxt, descriptionJson))
		addRibbonButtonModel(new RibbonButtonModel(this, descriptionJson));
}


QVariant RibbonModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();


	switch(role)
	{
	case DisplayRole:	return QString::fromStdString(_moduleNames.at(index.row()));
	case ClusterRole:	//To Do!
	default:			return QVariant();
	}
}


QHash<int, QByteArray> RibbonModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{ ClusterRole,		"clusterMenu"	},
		{ DisplayRole,		"displayText"	}
	};

	return roles;
}

RibbonButtonModel* RibbonModel::ribbonButtonModel(std::string name)
{
	if(_modulesByName.count(name) > 0)
		return _modulesByName[name];

	return NULL;
}

void RibbonModel::addRibbonButtonModel(RibbonButtonModel* model)
{
	//Should also somehow be represented in the plus-button of the tabbar... see void TabBar::addModulesPlusButton(). But maybe this is better to be done in QML

	if(isModuleName(model->title()))
		removeRibbonButtonModel(model->title());

	emit beginResetModel();

	_moduleNames.push_back(model->title());
	_modulesByName[model->title()] = model;

	emit endResetModel();
}

void RibbonModel::removeRibbonButtonModel(std::string moduleName)
{
	emit beginResetModel();

	if(!isModuleName(moduleName))
		return;

	delete _modulesByName[moduleName];
	_modulesByName.erase(moduleName);

	for(int i=_moduleNames.size() - 1; i>=0; i--)
		if(_moduleNames[i] == moduleName)
			_moduleNames.erase(_moduleNames.begin() + i);

	emit endResetModel();
}
