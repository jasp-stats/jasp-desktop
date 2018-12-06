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
#include "ribbonbuttonmodel.h"
#include "enginedefinitions.h"
#include "modules/dynamicmodule.h"

RibbonButtonModel::RibbonButtonModel(QObject *parent, Json::Value descriptionJson)  : QAbstractListModel(parent)
{
	try
	{
		Json::Value & moduleDescription = descriptionJson["moduleDescription"];

		setRequiresDataset(	moduleDescription.get("requiresDataset",	true).asBool()		);
		setDynamic(			moduleDescription.get("dynamic",			true).asBool()		);
		setTitle(			moduleDescription.get("title",				"???").asString()	);

		std::vector<Modules::RibbonEntry*>	ribbonEntries;

		for(Json::Value & ribbonEntry : descriptionJson["ribbonEntries"])
			ribbonEntries.push_back(new Modules::RibbonEntry(ribbonEntry, NULL));

		setRibbonEntries(ribbonEntries);
	}
	catch(std::exception e)
	{

		throw std::runtime_error("During the parsing of the description.json of the Module " + _title + " something went wrong: " + e.what());
	}
}

RibbonButtonModel::RibbonButtonModel(QObject *parent, Modules::DynamicModule * module)  : QAbstractListModel(parent)
{
	setRibbonEntries(	module->ribbonEntries()		);
	setTitle(			module->title()				);
	setDynamic(			true						);
	setRequiresDataset(	module->requiresDataset()	);
}


QVariant RibbonButtonModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	Modules::RibbonEntry* entry = _ribbonEntries.at(index.row());
    AnalysisMenuModel *menuModel = qobject_cast<AnalysisMenuModel *>(_analysisMenuModels.at(index.row()));

	switch(role)
	{
	case AnalysisMenuRole:      return QVariant::fromValue(menuModel);
	case DisplayRole:			return QString::fromStdString(entry->title());
	case IconSourceRole:		return QString::fromStdString(entry->icon());
	default:					return QVariant();
	}
}


QHash<int, QByteArray> RibbonButtonModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	AnalysisMenuRole,	"analysisMenu"	},
		{	DisplayRole,		"displayText"	},
		{	IconSourceRole,		"iconSource"	}
	};

	return roles;
}

void RibbonButtonModel::setRibbonEntries(Modules::RibbonEntries ribbonEntries)
{
	_ribbonEntries = ribbonEntries;

	for (auto * ribbonEntry : _ribbonEntries)
	{
		AnalysisMenuModel * model = new AnalysisMenuModel(this);
		model->setAnalysisEntries(ribbonEntry->analysisEntries());
		_analysisMenuModels.push_back(model);
    }
}
