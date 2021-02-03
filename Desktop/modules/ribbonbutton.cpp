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
#include "utilities/qutils.h"
#include "log.h"

using namespace Modules;

RibbonButton::RibbonButton(QObject *parent, DynamicModule * module)  : QObject(parent)
{
	setDynamicModule(module);

	setTitle(			_module->title()					);
	setRequiresData(	_module->requiresData()				);
	setIsCommon(		_module->isCommon()					);
	setModuleName(		_module->name()						);
	setIconSource(tq(	_module->iconFilePath())			);

	bindYourself();
}

void RibbonButton::setDynamicModule(DynamicModule * module)
{
	if(_module != module)
	{
		_module = module;
		connect(_module, &DynamicModule::descriptionReloaded,	this, &RibbonButton::reloadDynamicModule,	Qt::QueuedConnection);
		connect(_module, &DynamicModule::loadedChanged,			this, &RibbonButton::setReady			);
		connect(_module, &DynamicModule::errorChanged,			this, &RibbonButton::setError			);

		if(!_analysisMenuModel)
			_analysisMenuModel = new AnalysisMenuModel(this, _module);

		_analysisMenuModel->setDynamicModule(_module);

		setReady(false);
	}
}

void RibbonButton::reloadDynamicModule(DynamicModule * dynMod)
{
	bool dynamicModuleChanged = _module != dynMod;

	if(dynamicModuleChanged)
		setDynamicModule(dynMod);

	setTitle(			_module->title()		);
	setRequiresData(	_module->requiresData()	);
	setIconSource(tq(	_module->iconFilePath()));

	//if(dynamicModuleChanged)
	emit iChanged(this);
}

void RibbonButton::setError(bool error)
{
	if (_error == error)
		return;

	_error = error;
	emit errorChanged(_error);
}

void RibbonButton::setReady(bool ready)
{
	if (_ready == ready)
		return;

	Log::log() << "RibbonButton " << title() << " is " << (ready ? "" : "not ") << "ready!" << std::endl;

	_ready = ready;
	emit readyChanged(_ready);
}

RibbonButton::RibbonButton(QObject *parent,	std::string name, std::string title, std::string icon, bool requiresData, std::function<void ()> justThisFunction)
	: QObject(parent), _module(nullptr), _specialButtonFunc(justThisFunction)
{
	_analysisMenuModel = new AnalysisMenuModel(this, nullptr);
	setModuleName(name);
	setTitle(title);
	setIconSource(tq(icon));

	setRequiresData(requiresData); //setRequiresData because setMenu changes it based on the menu entries, but that doesnt work for this special dummy

	bindYourself();
}

void RibbonButton::bindYourself()
{
	connect(this,						&RibbonButton::enabledChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::titleChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::titleChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::moduleNameChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::dataLoadedChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::requiresDataChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::activeChanged,		this, &RibbonButton::somePropertyChanged);

	connect(this,						&RibbonButton::enabledChanged,		this, &RibbonButton::activeChanged		);
	connect(this,						&RibbonButton::dataLoadedChanged,	this, &RibbonButton::activeChanged		);
	connect(this,						&RibbonButton::requiresDataChanged,	this, &RibbonButton::activeChanged		);

	connect(DynamicModules::dynMods(),			&DynamicModules::dataLoadedChanged,	this, &RibbonButton::dataLoadedChanged	);
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
	Log::log() << "Iconsource ribbonbutton changed to: " << iconSource.toStdString() << std::endl;

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
		if(!isSpecial())
		{
			if(enabled)	DynamicModules::dynMods()->loadModule(moduleName());
			else		DynamicModules::dynMods()->unloadModule(moduleName());
		}

		emit DynamicModules::dynMods()->moduleEnabledChanged(moduleNameQ(), enabled);
	}
}

void RibbonButton::setIsCommon(bool isCommon)
{
	if (_isCommonModule == isCommon)
		return;

	_isCommonModule = isCommon;
	emit isCommonChanged();

	if(!_enabled && _isCommonModule)
		_enabled = true;
}

void RibbonButton::setModuleName(std::string moduleName)
{
	if (_moduleName == moduleName)
		return;

	_moduleName = moduleName;
	emit moduleNameChanged();
}

DynamicModule * RibbonButton::dynamicModule()
{
	return DynamicModules::dynMods()->dynamicModule(_moduleName);
}

AnalysisEntry *RibbonButton::getAnalysis(const std::string &name)
{
	AnalysisEntry* analysis = nullptr;
	analysis = _analysisMenuModel->getAnalysisEntry(name);
	
	return analysis;
}

std::vector<std::string> RibbonButton::getAllAnalysisNames() const
{
	std::vector<std::string> allAnalyses;
	for (AnalysisEntry* menuEntry : _module->menu())
		if (menuEntry->isAnalysis())
			allAnalyses.push_back(menuEntry->function());

	return allAnalyses;
}

void RibbonButton::setToolTip(QString toolTip)
{
	if (_toolTip == toolTip)
		return;

	_toolTip = toolTip;
	emit toolTipChanged(_toolTip);
}
