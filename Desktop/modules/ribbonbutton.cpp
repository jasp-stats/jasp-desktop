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
	setToolTip(		tq(	_module->description()	)			);
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
		connect(_module, &DynamicModule::readyChanged,			this, &RibbonButton::setReady,				Qt::QueuedConnection);
		connect(_module, &DynamicModule::errorChanged,			this, &RibbonButton::setError									);

		if(!_menuModel)
			_menuModel = new MenuModel(this, _module);

		_menuModel->setDynamicModule(_module);

		setReady(_module->installed());
	}
}

void RibbonButton::reloadDynamicModule(DynamicModule * dynMod)
{
	bool dynamicModuleChanged = _module != dynMod;

	if(dynamicModuleChanged)
		setDynamicModule(dynMod);

	setTitle(			_module->title()		);
	setToolTip(		tq(	_module->description())	);
	setRequiresData(	_module->requiresData()	);
	setIconSource(tq(	_module->iconFilePath()));
	setModuleName(		_module->name()			);

	emit iChanged(this);
}

void RibbonButton::setRemember(bool remember)
{
	if (_remember == remember)
		return;

	_remember = remember;
	emit rememberChanged(_remember);
}

void RibbonButton::runSpecial(QString func)
{
	if(_specialButtonFunc)	_specialButtonFunc();
	else					_menuModel->getAnalysisEntry(fq(func))->runSpecialFunc();
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
	
	if(_ready && dynamicModule() && dynamicModule()->isDevMod())
		setEnabled(true); 
}

RibbonButton::RibbonButton(QObject *parent,	std::string name, std::string title, std::string icon, bool requiresData, std::function<void ()> justThisFunction, std::string toolTip, bool enabled, bool remember)
	: QObject(parent), _enabled(enabled), _remember(remember), _special(true), _module(nullptr), _specialButtonFunc(justThisFunction)
{
	_menuModel = new MenuModel(this);

	setModuleName(name);
	setTitle(title);
	setToolTip(tq(toolTip));
	setIconSource(tq(icon));

	setRequiresData(requiresData); //setRequiresData because setMenu changes it based on the menu entries, but that doesnt work for this special dummy

	bindYourself();
}

RibbonButton::RibbonButton(QObject *parent, std::string name,	std::string title, std::string icon, Modules::AnalysisEntries * funcEntries, bool enabled, bool remember)
	: QObject(parent), _enabled(enabled), _remember(remember), _special(true), _module(nullptr)
{
	_menuModel = new MenuModel(this, funcEntries);

	setRequiresData(AnalysisEntry::requiresDataEntries(*funcEntries));
	setModuleName(name);
	setTitle(title);
	setIconSource(tq(icon));

	bindYourself();
}



void RibbonButton::bindYourself()
{
	connect(this,						&RibbonButton::enabledChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::titleChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::toolTipChanged,		this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::moduleNameChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::dataLoadedChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::requiresDataChanged,	this, &RibbonButton::somePropertyChanged);
	connect(this,						&RibbonButton::activeChanged,		this, &RibbonButton::somePropertyChanged);

	connect(this,						&RibbonButton::enabledChanged,		this, &RibbonButton::activeChanged		);
	connect(this,						&RibbonButton::dataLoadedChanged,	this, &RibbonButton::activeChanged		);
	connect(this,						&RibbonButton::requiresDataChanged,	this, &RibbonButton::activeChanged		);

	connect(DynamicModules::dynMods(),	&DynamicModules::dataLoadedChanged,	this, &RibbonButton::dataLoadedChanged	);
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
	//Log::log() << "Iconsource ribbonbutton changed to: " << iconSource.toStdString() << std::endl;

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
			if(enabled)	DynamicModules::dynMods()->loadModule(name());
			else		DynamicModules::dynMods()->unloadModule(name());
		}

		emit DynamicModules::dynMods()->moduleEnabledChanged(nameQ(), enabled);
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
	if (_name == moduleName)
		return;

	_name = moduleName;
	emit moduleNameChanged();
}

DynamicModule * RibbonButton::dynamicModule()
{
	return _module;
}

AnalysisEntry *RibbonButton::getEntry(const std::string &name)
{
	AnalysisEntry* analysis = nullptr;
	analysis = _menuModel->getAnalysisEntry(name);
	
	return analysis;
}

std::vector<std::string> RibbonButton::getAllEntries() const
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
