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


#include "ribbonbutton.h"
#include "enginedefinitions.h"
#include "modules/dynamicmodule.h"
#include "modules/analysisentry.h"

RibbonButton::RibbonButton(QObject *parent, Json::Value descriptionJson, bool isCommon)  : QObject(parent)
{
	_analysisMenuModel = new AnalysisMenuModel(this);
	try
	{
		Json::Value & moduleDescription = descriptionJson["moduleDescription"];

		setRequiresDataset(	moduleDescription.get("requiresDataset",	true).asBool()		);
		setIsDynamic(		moduleDescription.get("dynamic",			false).asBool()		); //It should never be dynamic here right?
		setIsCommon(		isCommon														);
		setTitle(			moduleDescription.get("title",				"???").asString()	);
		setModuleName(		title()															);
		setIconSource(QString::fromStdString(moduleDescription.get("icon", "").asString()));

		for(Json::Value & menuEntry : descriptionJson["menu"])
		{
#ifndef JASP_DEBUG
			if (menuEntry.get("debug", false).asBool())
				continue;
#endif
			_menuEntries.push_back(new Modules::AnalysisEntry(menuEntry, nullptr));
		}

		setMenu(_menuEntries);
	}
	catch(std::exception e)
	{
		throw std::runtime_error("During the parsing of the description.json of the Module " + _title + " something went wrong: " + e.what());
	}

	if (isCommon)
		setEnabled(true);

	bindYourself();
}

RibbonButton::RibbonButton(QObject *parent, Modules::DynamicModule * module)  : QObject(parent), _module(module)
{
	_analysisMenuModel = new AnalysisMenuModel(this);
	setMenu(			_module->menu()	);
	setTitle(			_module->title()			);
	setRequiresDataset(	_module->requiresDataset()	);
	setIsDynamic(		true						);
	setIsCommon(		false						);
	setModuleName(		_module->name()				);
	setIconSource(QString::fromStdString(_module->iconFilePath()));


	bindYourself();

	connect(_module, &Modules::DynamicModule::DynamicModule::descriptionReloaded, this, &RibbonButton::descriptionReloaded);
}

void RibbonButton::descriptionReloaded(Modules::DynamicModule * dynMod)
{
	setMenu(			_module->menu()	);
	setTitle(			_module->title()			);
	setRequiresDataset(	_module->requiresDataset()	);
}

void RibbonButton::bindYourself()
{
	connect(this, &RibbonButton::enabledChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this, &RibbonButton::titleChanged,			this, &RibbonButton::somePropertyChanged);
	connect(this, &RibbonButton::isDynamicChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this, &RibbonButton::titleChanged,			this, &RibbonButton::somePropertyChanged);
	connect(this, &RibbonButton::moduleNameChanged,		this, &RibbonButton::somePropertyChanged);
}

void RibbonButton::setMenu(const Modules::AnalysisEntries& entries)
{
	_analysisMenuModel->setAnalysisEntries(entries);

	emit analysisMenuChanged();
}

void RibbonButton::setRequiresDataset(bool requiresDataset)
{
	if(_requiresDataset == requiresDataset)
		return;

	_requiresDataset = requiresDataset;
	emit requiresDatasetChanged();
}

void RibbonButton::setTitle(std::string title)
{
	if(_title == title)
		return;

	_title = title;
	emit titleChanged();
}

void RibbonButton::setIconSource(QString iconSource)
{
	if(_iconSource == iconSource)
		return;

	_iconSource = iconSource;
	emit iconSourceChanged();
}

void RibbonButton::setEnabled(bool enabled)
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

void RibbonButton::setIsDynamic(bool isDynamic)
{
	if (_isDynamicModule == isDynamic)
		return;

	_isDynamicModule = isDynamic;
	emit isDynamicChanged();
}

void RibbonButton::setIsCommon(bool isCommon)
{
	if (_isCommonModule == isCommon)
		return;

	_isCommonModule = isCommon;
	emit isCommonChanged();
}

void RibbonButton::setModuleName(std::string moduleName)
{
	if (_moduleName == moduleName)
		return;

	_moduleName = moduleName;
	emit moduleNameChanged();
}

Modules::DynamicModule * RibbonButton::myDynamicModule()
{
	return !isDynamic() ? nullptr : _dynamicModules->dynamicModule(_moduleName);
}

Modules::AnalysisEntry *RibbonButton::getAnalysis(const std::string &name)
{
	Modules::AnalysisEntry* analysis = nullptr;
	analysis = _analysisMenuModel->getAnalysisEntry(name);
	
	return analysis;
}

std::vector<std::string> RibbonButton::getAllAnalysisNames() const
{
	std::vector<std::string> allAnalyses;
	for (Modules::AnalysisEntry* menuEntry : _menuEntries)
	{
		if (menuEntry->isAnalysis())
			allAnalyses.push_back(menuEntry->function());
	}

	return allAnalyses;
}
