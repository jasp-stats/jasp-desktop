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
#include "modules/analysisentry.h"

RibbonButtonModel::RibbonButtonModel(QObject *parent, Json::Value descriptionJson)  : QAbstractListModel(parent)
{
	try
	{
		Json::Value & moduleDescription = descriptionJson["moduleDescription"];

		setRequiresDataset(	moduleDescription.get("requiresDataset",	true).asBool()		);
		setIsDynamic(		moduleDescription.get("dynamic",			false).asBool()		); //It should never be dynamic here right?
		setTitle(			moduleDescription.get("title",				"???").asString()	);
		setModuleName(		title()															);

		std::vector<Modules::RibbonEntry*>	ribbonEntries;

		for(Json::Value & ribbonEntry : descriptionJson["ribbonEntries"])
		{
#ifndef JASP_DEBUG
			if (ribbonEntry.get("debug", false).asBool())
				continue;
#endif
			ribbonEntries.push_back(new Modules::RibbonEntry(ribbonEntry, nullptr));
		}

		setRibbonEntries(ribbonEntries);
	}
	catch(std::exception e)
	{
		throw std::runtime_error("During the parsing of the description.json of the Module " + _title + " something went wrong: " + e.what());
	}

	if(title() == "Common")
		setEnabled(true);

	bindYourself();
}

RibbonButtonModel::RibbonButtonModel(QObject *parent, Modules::DynamicModule * module)  : QAbstractListModel(parent), _module(module)
{
	setRibbonEntries(	_module->ribbonEntries()	);
	setTitle(			_module->title()			);
	setRequiresDataset(	_module->requiresDataset()	);
	setIsDynamic(		true						);
	setModuleName(		_module->name()				);

	bindYourself();

	connect(_module, &Modules::DynamicModule::DynamicModule::descriptionReloaded, this, &RibbonButtonModel::descriptionReloaded);
}

void RibbonButtonModel::descriptionReloaded(Modules::DynamicModule * dynMod)
{
	beginResetModel();
	setRibbonEntries(	_module->ribbonEntries()	);
	setTitle(			_module->title()			);
	setRequiresDataset(	_module->requiresDataset()	);
	endResetModel();
}

void RibbonButtonModel::bindYourself()
{
	connect(this, &RibbonButtonModel::enabledChanged,		this, &RibbonButtonModel::somePropertyChanged);
	connect(this, &RibbonButtonModel::titleChanged,			this, &RibbonButtonModel::somePropertyChanged);
	connect(this, &RibbonButtonModel::isDynamicChanged,		this, &RibbonButtonModel::somePropertyChanged);
	connect(this, &RibbonButtonModel::titleChanged,			this, &RibbonButtonModel::somePropertyChanged);
	connect(this, &RibbonButtonModel::moduleNameChanged,	this, &RibbonButtonModel::somePropertyChanged);
}


QVariant RibbonButtonModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	Modules::RibbonEntry* entry = _ribbonEntries.at(index.row());
	AnalysisMenuModel *menuModel = qobject_cast<AnalysisMenuModel *>(_analysisMenuModels.at(index.row()));

	switch(role)
	{
	case AnalysisMenuRole:		return QVariant::fromValue(menuModel);
	case DisplayRole:			return QString::fromStdString(entry->title());
	case IconSourceRole:		return QString::fromStdString(entry->icon());
	case EnabledAnalysisRole:	{std::cout << "EnabledAnalysisRole not implemented!" << std::endl; return true; }
	default:					return QVariant();
	}
}


QHash<int, QByteArray> RibbonButtonModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{	AnalysisMenuRole,		"analysisMenu"			},
		{	DisplayRole,			"displayText"			},
		{	IconSourceRole,			"iconSource"			},
		{	EnabledAnalysisRole,	"analysisEnabled"		}

	};

	return roles;
}

void RibbonButtonModel::setRibbonEntries(Modules::RibbonEntries ribbonEntries)
{
	for(auto * oldMenuModel : _analysisMenuModels)
		delete oldMenuModel;
	_analysisMenuModels.clear();

	_ribbonEntries = ribbonEntries;

	for (auto * ribbonEntry : _ribbonEntries)
	{
		AnalysisMenuModel * model = new AnalysisMenuModel(this);
		model->setAnalysisEntries(ribbonEntry->analysisEntries());
		_analysisMenuModels.push_back(model);
    }
}

void RibbonButtonModel::setRequiresDataset(bool requiresDataset)
{
	if(_requiresDataset == requiresDataset)
		return;

	_requiresDataset = requiresDataset;
	emit requiresDatasetChanged();
}

void RibbonButtonModel::setTitle(std::string title)
{
	if(_title == title)
		return;

	_title = title;
	emit titleChanged();
}

void RibbonButtonModel::setEnabled(bool enabled)
{
	if (_enabled == enabled)
		return;

	_enabled = enabled;
	emit enabledChanged();

	if(isDynamic())
	{
		if(enabled)	_dynamicModules->loadModule(moduleName());
		else		_dynamicModules->unloadModule(moduleName());

	}
}

void RibbonButtonModel::setIsDynamic(bool isDynamic)
{
	if (_isDynamicModule == isDynamic)
		return;

	_isDynamicModule = isDynamic;
	emit isDynamicChanged();
}

void RibbonButtonModel::setModuleName(std::string moduleName)
{
	if (_moduleName == moduleName)
		return;

	_moduleName = moduleName;
	emit moduleNameChanged();
}

Modules::DynamicModule * RibbonButtonModel::myDynamicModule()
{
	return !isDynamic() ? nullptr : _dynamicModules->dynamicModule(_moduleName);
}

Modules::AnalysisEntry *RibbonButtonModel::getAnalysis(const std::string &name)
{
	Modules::AnalysisEntry* analysis = nullptr;
	for (AnalysisMenuModel* analysisMenu : _analysisMenuModels)
	{
		analysis = analysisMenu->getAnalysisEntry(name);
		if (analysis)
			break;
	}
	
	return analysis;
}
