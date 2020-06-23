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
#include "utilities/languagemodel.h"

RibbonButton::RibbonButton(QObject *parent, Json::Value descriptionJson, bool isCommon)  : QObject(parent)
{
	_analysisMenuModel = new AnalysisMenuModel(this);
	try
	{
		Json::Value & moduleDescription = descriptionJson["moduleDescription"];

		setRequiresData(	moduleDescription.get("requiresData",		true).asBool()		);
		setIsDynamic(		moduleDescription.get("dynamic",			false).asBool()		); //It should never be dynamic here right?
		setIsCommon(		isCommon														);
		setTitle(			moduleDescription.get("title",				"???").asString()	);
		setModuleName(		moduleDescription.get("name",				title()).asString()	);
		setIconSource(QString::fromStdString(moduleDescription.get("icon", "").asString()));

		bool defaultRequiresDataset = _requiresData;

		for(Json::Value & menuEntry : descriptionJson["menu"])
		{
#ifndef JASP_DEBUG
			if (menuEntry.get("debug", false).asBool())
				continue;
#endif
			Modules::AnalysisEntry * entry = new Modules::AnalysisEntry(menuEntry, nullptr, defaultRequiresDataset);
			_menuEntries.push_back(entry);

			if(!entry->requiresData())
				setRequiresData(false);
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
	setRequiresData(	_module->requiresData()	);
	setIsDynamic(		true						);
	setIsCommon(		false						);
	setModuleName(		_module->name()				);
	setIconSource(QString::fromStdString(_module->iconFilePath()));


	bindYourself();

	connect(_module, &Modules::DynamicModule::DynamicModule::descriptionReloaded, this, &RibbonButton::descriptionReloaded);
}

RibbonButton::RibbonButton(QObject *parent,	std::string name, std::string title, std::string icon, bool requiresData, std::function<void ()> justThisFunction)
	: QObject(parent), _module(nullptr), _specialButtonFunc(justThisFunction)
{
	_analysisMenuModel = new AnalysisMenuModel(this);
	setModuleName(name);
	setTitle(title);
	setIconSource(tq(icon));
	setRequiresData(requiresData);
	setIsDynamic(false);

	setMenu({ new Modules::AnalysisEntry() }); //Just a single dummy

	bindYourself();
}


void RibbonButton::descriptionReloaded(Modules::DynamicModule * dynMod)
{
	assert(dynMod == _module);

	setMenu(			_module->menu()	);
	setTitle(			_module->title()			);
	setRequiresData(	_module->requiresData()	);
}

void RibbonButton::bindYourself()
{
	connect(this,						&RibbonButton::enabledChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::titleChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::isDynamicChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::titleChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::moduleNameChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::dataLoadedChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::requiresDataChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::activeChanged,		this, &RibbonButton::somePropertyChanged);

	connect(this,						&RibbonButton::enabledChanged,		this, &RibbonButton::activeChanged		);
	connect(this,						&RibbonButton::dataLoadedChanged,	this, &RibbonButton::activeChanged		);
	connect(this,						&RibbonButton::requiresDataChanged,	this, &RibbonButton::activeChanged		);

	connect(DynamicModules::dynMods(),	&DynamicModules::dataLoadedChanged,	this, &RibbonButton::dataLoadedChanged	);
}

void RibbonButton::setMenu(const Modules::AnalysisEntries& entries)
{
	_analysisMenuModel->setAnalysisEntries(entries);

	emit analysisMenuChanged();
}

void RibbonButton::setRequiresData(bool requiresDataset)
{
	if(_requiresData == requiresDataset)
		return;

	_requiresData = requiresDataset;
	emit requiresDataChanged();
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

	if(DynamicModules::dynMods())
	{

		if(isDynamic())
		{
			if(enabled)	DynamicModules::dynMods()->loadModule(moduleName());
			else		DynamicModules::dynMods()->unloadModule(moduleName());

		}

		emit DynamicModules::dynMods()->moduleEnabledChanged(moduleNameQ(), enabled);
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
	return !isDynamic() ? nullptr : DynamicModules::dynMods()->dynamicModule(_moduleName);
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

void RibbonButton::reloadMenuFromDescriptionJson()
{
	//Find the location of the module path
	QFileInfo modulepath = QFileInfo(QString::fromStdString(Dirs::resourcesDir() + _moduleName + "/"));
	if(!modulepath.exists())
	{
		Log::log() << "Path " << modulepath.absoluteFilePath().toStdString() << " does not exist!" << std::endl << std::flush;
		return;
	}

	//Check existence of the description.json
	QFile descriptionFile(modulepath.absoluteFilePath() + "/" + getJsonDescriptionFilename());
	if(!descriptionFile.exists())
	{
		Log::log() << "Could not find the json description file : " << descriptionFile.fileName().toStdString() << std::endl;
		return;
	}

	//Open the description.json
	if (!descriptionFile.open(QFile::ReadOnly))
	{
		Log::log() << "Could not open the json description file : " << descriptionFile.fileName().toStdString()  << std::endl;
		return;

	}

	//Parse the description.json
	std::string	descriptionTxt(descriptionFile.readAll().toStdString());
	Json::Value descriptionJson;
	if (!Json::Reader().parse(descriptionTxt, descriptionJson))
	{
		Log::log() << "Could not parse description.json file in " << descriptionFile.fileName().toStdString() << std::endl;
		return;
	}

	Modules::AnalysisEntries saveMenuEntries = _menuEntries;

	//Find the menu entries in the description.json
	try
	{
		_menuEntries.clear();
		Json::Value & moduleDescription = descriptionJson["moduleDescription"];
		_title = moduleDescription["title"].asString();
		int i = 0;

		for(Json::Value & menuEntry : descriptionJson["menu"])
		{
#ifndef JASP_DEBUG
			if (menuEntry.get("debug", false).asBool())
				continue;
#endif
			Modules::AnalysisEntry * entry = new Modules::AnalysisEntry(menuEntry, nullptr, _requiresData);
			_menuEntries.push_back(entry);
			if (i < saveMenuEntries.size())
				emit analysisTitleChanged(_moduleName, saveMenuEntries[i]->title() , entry->title());
			i++;
		}
		setMenu(_menuEntries);
	}
	catch(std::exception e)
	{
		Log::log() << "During the parsing of the description.json of the Module " << _title  << " something went wrong: " <<  e.what() << std::endl;
		_menuEntries = saveMenuEntries;
	}

}

QString RibbonButton::getJsonDescriptionFilename()
{
	return "description" + LanguageModel::currentTranslationSuffix() + ".json";
}
