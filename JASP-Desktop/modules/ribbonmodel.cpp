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
#include "dirs.h"
#include "log.h"


RibbonModel::RibbonModel(std::vector<std::string> commonModulesToLoad, std::vector<std::string> extraModulesToLoad)
	: QAbstractListModel(DynamicModules::dynMods())
{

	for(const std::string & moduleName : commonModulesToLoad)
		addRibbonButtonModelFromModulePath(QFileInfo(QString::fromStdString(Dirs::resourcesDir() + moduleName + "/")), true);

	for(const std::string & moduleName : extraModulesToLoad)
		addRibbonButtonModelFromModulePath(QFileInfo(QString::fromStdString(Dirs::resourcesDir() + moduleName + "/")), false);

	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleAdded,			this, &RibbonModel::addDynamicRibbonButtonModel);
	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleUninstalled,	this, &RibbonModel::removeDynamicRibbonButtonModel);

	for(const std::string & modName : DynamicModules::dynMods()->moduleNames())
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
	addRibbonButtonModel(new RibbonButton(this, module));
}

void RibbonModel::addRibbonButtonRPrompt()
{
	addRibbonButtonModel(new RibbonButton(this, "R", fq(tr("R (Beta)")), "Rlogo.svg", false, [&](){ emit showRCommander(); }));
}

void RibbonModel::addRibbonButtonModelFromModulePath(QFileInfo modulePath, bool isCommon)
{
	if(!modulePath.exists())
	{
		Log::log() << "Path " << modulePath.absoluteFilePath().toStdString() << " does not exist!" << std::endl;
		return;
	}

	QFile descriptionFile(modulePath.absoluteFilePath() + "/" + RibbonButton::getJsonDescriptionFilename());
	if(!descriptionFile.exists())
	{
		Log::log()	<< "Could not find "  << RibbonButton::getJsonDescriptionFilename() << " file in " << modulePath.absoluteFilePath().toStdString()
					<< "\nTry to fall back to original description.json. " << std::endl;
		descriptionFile.setFileName(modulePath.absoluteFilePath() + "/" + "description.json");
		if(!descriptionFile.exists())
		{
			Log::log() << "No description.json file found for this module." << std::endl; //Shouldn't this give an error?
			return;
		}
	}

	descriptionFile.open(QFile::ReadOnly);
	std::string	descriptionTxt(descriptionFile.readAll().toStdString());

	Json::Value descriptionJson;

	try
	{
		if(Json::Reader().parse(descriptionTxt, descriptionJson))
			addRibbonButtonModel(new RibbonButton(this, descriptionJson, isCommon));
		else
			Log::log() << "Error while reading description.json of " << modulePath.filePath().toStdString() << std::endl;
	}
	catch(std::runtime_error e)
	{
		Log::log() << e.what() << std::endl;
	}
}

void RibbonModel::addRibbonButtonModel(RibbonButton* model)
{
	if(isModuleName(model->moduleName()))
		removeRibbonButtonModel(model->moduleName());

	emit beginInsertRows(QModelIndex(), rowCount(), rowCount());

	_moduleNames.push_back(model->moduleName());
	_buttonModelsByName[model->moduleName()] = model;

	emit endInsertRows();

	connect(model, &RibbonButton::iChanged, this, &RibbonModel::ribbonButtonModelChanged);
	connect(model, &RibbonButton::analysisTitleChanged, this, &RibbonModel::analysisTitleChanged);
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
	case DynamicRole:		return ribbonButtonModelAt(row)->isDynamic();
	case CommonRole:		return ribbonButtonModelAt(row)->isCommon();
	case ModuleNameRole:	return ribbonButtonModelAt(row)->moduleNameQ();
	case ModuleRole:		return QVariant::fromValue(ribbonButtonModelAt(row)->myDynamicModule());
	case ClusterRole:		//To Do!
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
		{ DynamicRole,		"isDynamic"			},
		{ CommonRole,		"isCommon"			},
		{ ModuleNameRole,	"moduleName"		},
		{ ModuleTitleRole,	"moduleTitle"		},
		{ ModuleRole,		"dynamicModule"		},
		{ ActiveRole,		"active"			} };

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

void RibbonModel::refresh()
{

	beginResetModel();

	for (const std::string& myModuleName : _moduleNames)
	{
		RibbonButton* button = _buttonModelsByName[myModuleName];
		button->reloadMenuFromDescriptionJson();
	}

	endResetModel();

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

Modules::AnalysisEntry *RibbonModel::getAnalysis(const std::string& moduleName, const std::string& analysisName)
{
	Modules::AnalysisEntry* analysis = nullptr;
	RibbonButton* ribbonButton = ribbonButtonModel(moduleName);
	if (ribbonButton)
		analysis = ribbonButton->getAnalysis(analysisName);
	
	return analysis;
}

QString RibbonModel::getModuleNameFromAnalysisName(const QString analysisName)
{
	QString result = "Common";
	std::string searchName = analysisName.toStdString();
	// This function is needed for old JASP file: they still have a reference to the common mondule that does not exist anymore.
	for (const std::string& myModuleName : _moduleNames)
	{
		RibbonButton* button = _buttonModelsByName[myModuleName];
		for (const std::string& name : button->getAllAnalysisNames())
		{
			if (name == searchName)
			{
				result = QString::fromStdString(myModuleName);
				break;
			}
		}
		if (result != "Common")
			break;

	}

	return result;
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
