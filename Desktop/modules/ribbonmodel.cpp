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


#include "ribbonmodel.h"
#include "utilities/appdirs.h"
#include "gui/messageforwarder.h"
#include "log.h"

using namespace Modules;

RibbonModel * RibbonModel::_singleton = nullptr;

RibbonModel::RibbonModel() : QAbstractListModel(DynamicModules::dynMods())
{
	if(_singleton) throw std::runtime_error("RibbonModel can only be instantiated once!");
	_singleton = this;	

	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleAdded,			this, &RibbonModel::addDynamicRibbonButtonModel		);
	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleUninstalled,	this, &RibbonModel::removeDynamicRibbonButtonModel	);
	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleReplaced,		this, &RibbonModel::dynamicModuleReplaced			);
	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleChanged,		this, &RibbonModel::dynamicModuleChanged			);
}

void RibbonModel::loadModules(std::vector<std::string> commonModulesToLoad, std::vector<std::string> extraModulesToLoad)
{
	DynamicModules::dynMods()->initializeInstalledModules();	
	DynamicModules::dynMods()->insertCommonModuleNames(std::set<std::string>(commonModulesToLoad.begin(), commonModulesToLoad.end()));

	auto loadModulesFromBundledOrUserData = [&](bool common)
	{
		for(const std::string & moduleName : (common ? commonModulesToLoad : extraModulesToLoad))
		{
			if(DynamicModules::dynMods()->moduleIsInstalledByUser(moduleName)) //Only load bundled if the user did not install a newer/other version
				addRibbonButtonModelFromDynamicModule((*DynamicModules::dynMods())[moduleName]);
			else
			{
				try {
					std::string moduleLibrary = DynamicModules::bundledModuleLibraryPath(moduleName);
					DynamicModules::dynMods()->initializeModuleFromDir(moduleLibrary, true, common);
				} 
				catch (std::runtime_error & e) 
				{
					MessageForwarder::showWarning(tr("Loading bundled module %1 failed").arg(tq(moduleName)), tr("Loading of the bundled module %1 failed with the following error:\n\n%2").arg(tq(moduleName)).arg(tq(e.what())));
				}
			}
		}
	};
	
	loadModulesFromBundledOrUserData(true);
	loadModulesFromBundledOrUserData(false);
	
	for(const std::string & modName : DynamicModules::dynMods()->moduleNames())
		if(!isModuleName(modName)) //Was it already added from commonModulesToLoad or extraModulesToLoad?
			addRibbonButtonModelFromDynamicModule((*DynamicModules::dynMods())[modName]);

	addRibbonButtonRPrompt();

	if(PreferencesModel::prefs()->modulesRemember())
	{
		QStringList enabledModules = PreferencesModel::prefs()->modulesRemembered();

		for(const QString & enabledModule : enabledModules)
		{
			std::string mod = enabledModule.toStdString();

			if(_buttonModelsByName.count(mod) > 0)
				_buttonModelsByName[mod]->setEnabled(true);
		}
	}
}

void RibbonModel::addRibbonButtonModelFromDynamicModule(Modules::DynamicModule * module)
{
	RibbonButton * button = new RibbonButton(this, module);

	addRibbonButtonModel(button);
}

void RibbonModel::addRibbonButtonRPrompt()
{
	addRibbonButtonModel(new RibbonButton(this, "R", fq(tr("R (Beta)")), "Rlogo.svg", false, [&](){ emit showRCommander(); }, fq(tr("Execute R code in a console"))));
}

void RibbonModel::dynamicModuleChanged(Modules::DynamicModule * dynMod)
{
	Log::log() << "void RibbonModel::dynamicModuleChanged(" << dynMod->toString() << ")" << std::endl;

	for(const auto & nameButton : _buttonModelsByName)
		if(nameButton.second->dynamicModule() == dynMod)
			nameButton.second->reloadDynamicModule(dynMod);
}

void RibbonModel::addRibbonButtonModel(RibbonButton* model)
{
	if(isModuleName(model->moduleName()))
		removeRibbonButtonModel(model->moduleName());

	emit beginInsertRows(QModelIndex(), rowCount(), rowCount());

	_moduleNames.push_back(model->moduleName());
	_buttonModelsByName[model->moduleName()] = model;

	emit endInsertRows();

	connect(model, &RibbonButton::iChanged,				this, &RibbonModel::ribbonButtonModelChanged);
}

void RibbonModel::dynamicModuleReplaced(Modules::DynamicModule * oldModule, Modules::DynamicModule * module)
{
	for(const auto & nameButton : _buttonModelsByName)
		if(nameButton.second->dynamicModule() == oldModule || nameButton.first == oldModule->name())
			nameButton.second->reloadDynamicModule(module);
}

QVariant RibbonModel::data(const QModelIndex &index, int role) const
{
	if (index.row() >= rowCount())
		return QVariant();

	size_t row = size_t(index.row());

	switch(role)
	{
	case DisplayRole:													//Displayed in + panel
	case ModuleTitleRole:	return ribbonButtonModelAt(row)->titleQ();	//Displayed in ribbon. Could obviously be merged with the above
	case RibbonRole:		return QVariant::fromValue(ribbonButtonModelAt(row));
	case EnabledRole:		return ribbonButtonModelAt(row)->enabled();
	case ActiveRole:		return ribbonButtonModelAt(row)->active();
	case CommonRole:		return ribbonButtonModelAt(row)->isCommon();
	case ModuleNameRole:	return ribbonButtonModelAt(row)->moduleNameQ();
	case ModuleRole:		return QVariant::fromValue(ribbonButtonModelAt(row)->dynamicModule());
	case BundledRole:		return ribbonButtonModelAt(row)->isBundled();
	case VersionRole:		return ribbonButtonModelAt(row)->version();
	case SpecialRole:		return ribbonButtonModelAt(row)->isSpecial();
	case ClusterRole:		//To Do!?
	default:				return QVariant();
	}
}


QHash<int, QByteArray> RibbonModel::roleNames() const
{
	static const auto roles = QHash<int, QByteArray>{
		{ ClusterRole,		"clusterMenu"		},
		{ DisplayRole,		"displayText"		},
		{ RibbonRole,		"ribbonButton"		},
		{ EnabledRole,		"ribbonEnabled"		},
		{ CommonRole,		"isCommon"			},
		{ ModuleNameRole,	"moduleName"		},
		{ ModuleTitleRole,	"moduleTitle"		},
		{ ModuleRole,		"dynamicModule"		},
		{ ActiveRole,		"active"			},
		{ BundledRole,		"isBundled"			},
		{ VersionRole,		"moduleVersion"		},
		{ SpecialRole,		"isSpecial"			}
	};

	return roles;
}

RibbonButton* RibbonModel::ribbonButtonModel(std::string name) const
{
	if(_buttonModelsByName.count(name) > 0)
		return _buttonModelsByName.at(name);

	return nullptr;
}

void RibbonModel::removeRibbonButtonModel(std::string moduleName)
{
	if(!isModuleName(moduleName))
		return;

	int indexRemoved = -1;

	for(int i=_moduleNames.size() - 1; i >= 0; i--)
		if(_moduleNames[i] == moduleName)
		{
			indexRemoved = i;
			break;
		}

	emit beginRemoveRows(QModelIndex(), indexRemoved, indexRemoved);

	delete _buttonModelsByName[moduleName];
	_buttonModelsByName.erase(moduleName);

	_moduleNames.erase(_moduleNames.begin() + indexRemoved);

	emit endRemoveRows();
}

void RibbonModel::analysisClicked(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module)
{
	RibbonButton * button = ribbonButtonModel(fq(module));

	if(button->isSpecial())		button->runSpecial();
	else						emit analysisClickedSignal(analysisFunction, analysisQML, analysisTitle, module);
}

void RibbonModel::setHighlightedModuleIndex(int highlightedModuleIndex)
{
	if (_highlightedModuleIndex == highlightedModuleIndex)
		return;

	_highlightedModuleIndex = highlightedModuleIndex;
	emit highlightedModuleIndexChanged(_highlightedModuleIndex);
}

void RibbonModel::setModuleEnabled(int ribbonButtonModelIndex, bool enabled)
{
	if(ribbonButtonModelIndex < 0)
		return;

	RibbonButton * ribbonButtonModel = ribbonButtonModelAt(size_t(ribbonButtonModelIndex));

	if(ribbonButtonModel->enabled() != enabled)
	{
		ribbonButtonModel->setEnabled(enabled);
		emit dataChanged(index(ribbonButtonModelIndex), index(ribbonButtonModelIndex));
	}
}

QStringList RibbonModel::getModulesEnabled() const
{
	QStringList list;
	
	for(auto nameButton : _buttonModelsByName)
		if(nameButton.second->enabled())
			list.append(nameButton.second->moduleNameQ());
	
	return list;
}


Modules::AnalysisEntry *RibbonModel::getAnalysis(std::string moduleName, const std::string & analysisName)
{
	if(moduleName == "Common")
		moduleName = getModuleNameFromAnalysisName(analysisName);

	RibbonButton			* ribbonButton	= ribbonButtonModel(moduleName);

	if (ribbonButton)
		return ribbonButton->getAnalysis(analysisName);
	else
	{
		std::string strippedModuleName = stringUtils::stripNonAlphaNum(moduleName);

		if(strippedModuleName != moduleName)
			return getAnalysis(strippedModuleName, analysisName);
		else if(strippedModuleName.size() < 4 || strippedModuleName.substr(0, 4) != "jasp") //Cause we renamed everything to jasp*
			return getAnalysis("jasp" + strippedModuleName, analysisName);
	}

	return nullptr;
}

std::string RibbonModel::getModuleNameFromAnalysisName(const std::string & analysisName)
{
	// This function is needed for old JASP file: they still have a reference to the common module that does not exist anymore.
	for (const std::string & moduleName : _moduleNames)
		for (const std::string & name : _buttonModelsByName[moduleName]->getAllAnalysisNames())
			if (name == analysisName)
				return moduleName;

	return "Common";
}

void RibbonModel::toggleModuleEnabled(int ribbonButtonModelIndex)
{
	if(ribbonButtonModelIndex < 0)
		return;

	RibbonButton * ribbonButtonModel = ribbonButtonModelAt(size_t(ribbonButtonModelIndex));

	ribbonButtonModel->setEnabled(!ribbonButtonModel->enabled());

	emit dataChanged(index(ribbonButtonModelIndex), index(ribbonButtonModelIndex));
}

int RibbonModel::ribbonButtonModelIndex(RibbonButton * model)	const
{
	for(auto & keyval : _buttonModelsByName)
		if(keyval.second == model)
			for(size_t i=0; i<_moduleNames.size(); i++)
				if(_moduleNames[i] == keyval.first)
					return int(i);
	return -1;
}


void RibbonModel::ribbonButtonModelChanged(RibbonButton* model)
{
	int row = ribbonButtonModelIndex(model);
	emit dataChanged(index(row), index(row));
}

void RibbonModel::moduleLoadingSucceeded(const QString & moduleName)
{
	if(moduleName == "*")
		return;

	RibbonButton * ribMod = ribbonButtonModel(moduleName.toStdString());
	ribMod->setEnabled(true);
}
