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
#include "utilities/messageforwarder.h"
#include "utilities/settings.h"
#include "log.h"
#include "qquick/datasetview.h"
#include "mainwindow.h"
#include "installedmodules.h"

using namespace Modules;

RibbonModel * RibbonModel::_singleton = nullptr;

RibbonModel::RibbonModel() : QAbstractListModel(DynamicModules::dynMods())
{
	if(_singleton) throw std::runtime_error("RibbonModel can only be instantiated once!");
	_singleton = this;	

	_buttonNames = std::vector<stringvec>(2); //Analyses & Data

	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleAdded,			this, &RibbonModel::addRibbonButtonModelFromDynamicModule	);
	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleUninstalled,	this, &RibbonModel::removeDynamicRibbonButtonModel			);
	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleReplaced,		this, &RibbonModel::dynamicModuleReplaced					);
	connect(DynamicModules::dynMods(), &DynamicModules::dynamicModuleChanged,		this, &RibbonModel::dynamicModuleChanged					);
	connect(PreferencesModel::prefs(), &PreferencesModel::languageCodeChanged,		this, &RibbonModel::refresh									);
	connect(DataSetPackage::pkg(),	   &DataSetPackage::dataModeChanged,			this, &RibbonModel::setDataMode								);

}

void RibbonModel::loadModules(std::vector<InstalledModules::ModuleInfo> modulesToLoad)
{
	addSpecialRibbonButtonsEarly();
	
	std::set<std::string> commonNames = {};

	for(const auto& module : modulesToLoad) {
		try {
			if(module.common) commonNames.insert(module.name);
			DynamicModules::dynMods()->initializeModuleFromDir(module.libpath, module.bundled, module.common);
		}
		catch (std::runtime_error & e)
		{
			QString titleWarn = tr("Loading bundled module %1 failed").arg(tq(module.name)),
				bodyWarn  = tr("Loading of the bundled module %1 failed with the following error:\n\n%2").arg(tq(module.name)).arg(tq(e.what()));

			Log::log() << titleWarn << "\n" << bodyWarn << std::endl;

			MessageForwarder::showWarning(titleWarn, bodyWarn);
		}
	}
	DynamicModules::dynMods()->insertCommonModuleNames(commonNames);
	
	for(const std::string & modName : DynamicModules::dynMods()->moduleNames())
		if(!isModuleName(modName)) //Was it already added from commonModulesToLoad or extraModulesToLoad?
			addRibbonButtonModelFromDynamicModule((*DynamicModules::dynMods())[modName]);

	addSpecialRibbonButtonsLate();

	//All modules, common (core) ones included, can be (de)selected by the user in the modules-menu.
	//The common-modules are merely the initial default set of enabled modules (possibly overridden by an admin through OverrideCommon, which is likewise only an initial state).
	//If the user chose to remember enabled modules and ever stored a selection then that list is leading, otherwise we start out with the common modules enabled.
	QStringList		enabledModules	= PreferencesModel::prefs()->modulesRemembered();
	bool		selectionStored	= PreferencesModel::prefs()->modulesRemember() && Settings::isSet(Settings::MODULES_REMEMBERED);

	if(selectionStored && !Settings::isSet(Settings::MODULES_SELECTION_MIGRATED))
	{
		//One-time migration for selections stored before all modules became (de)selectable:
		//those lists never contained the common modules (those were force-enabled and so never stored), so add them to prevent the core modules disappearing from the ribbon upon upgrading.
		//The enabled-true-calls below store the added modules in the selection automatically.
		for(auto & nameButton : _buttonModelsByName)
			if(nameButton.second->remember() && nameButton.second->isCommon() && !enabledModules.contains(nameButton.second->nameQ()))
				enabledModules.append(nameButton.second->nameQ());

		Settings::setValue(Settings::MODULES_SELECTION_MIGRATED, true);
	}

	for(auto & nameButton : _buttonModelsByName)
	{
		RibbonButton * button = nameButton.second;

		if(!button->remember())
			continue;

		button->setEnabled(selectionStored ? enabledModules.contains(button->nameQ()) : button->isCommon());
	}

	//Apply any stored module-order last: order is independent of enabled-state and disabled modules simply keep their position, so they reappear where they were.
	QStringList storedOrder = Settings::value(Settings::MODULES_ORDER).toString().split("|", Qt::SkipEmptyParts);
	if(!storedOrder.isEmpty())
		setModuleOrder(storedOrder);
}

void RibbonModel::addRibbonButtonModelFromDynamicModule(Modules::DynamicModule * module)
{
	addRibbonButtonModel(new RibbonButton(this, module), size_t(RowType::Analyses));
}

void RibbonModel::addSpecialRibbonButtonsEarly()
{
	//_entriesInsert and _entriesDelete are destroyed by the menumodel destructor when the button gets destroyed.
	_entriesInsert = new AnalysisEntries(
	{
		new AnalysisEntry([&](){ emit this->dataInsertRowBefore(	-1);				},	"insert-row-before",			[&](){ return fq(tr("Insert row above"));},								true,		"menu-row-insert-before"),
		new AnalysisEntry([&](){ emit this->dataInsertRowAfter(		-1);				},	"insert-row-after",				[&](){ return fq(tr("Insert row below"));},								true,		"menu-row-insert-after"),
		new AnalysisEntry(),
		new AnalysisEntry([&](){ emit this->dataInsertColumnBefore(	-1,false,false);	},	"insert-column-before",			[&](){ return fq(tr("Insert column before"));},							true,		"menu-column-insert-before"),
		new AnalysisEntry([&](){ emit this->dataInsertColumnAfter(	-1,false,false);	},	"insert-column-after",			[&](){ return fq(tr("Insert column after"));},							true,		"menu-column-insert-after"),
		new AnalysisEntry(),
		new AnalysisEntry([&](){ emit this->dataInsertColumnBefore(	-1,true	,false);	},	"insert-c-column-before",		[&](){ return fq(tr("Insert constructor column before"));},				true,		"menu-column-insert-before"),
		new AnalysisEntry([&](){ emit this->dataInsertColumnAfter(	-1,true	,false);	},	"insert-c-column-after",		[&](){ return fq(tr("Insert constructor column after"));},				true,		"menu-column-insert-after"),
		new AnalysisEntry([&](){ emit this->dataInsertColumnBefore(	-1,true	,true);		},	"insert-r-column-before",		[&](){ return fq(tr("Insert R column before"));},						true,		"menu-column-insert-before"),
		new AnalysisEntry([&](){ emit this->dataInsertColumnAfter(	-1,true	,true);		},	"insert-r-column-after",		[&](){ return fq(tr("Insert R column after"));},						true,		"menu-column-insert-after"),
	});
	
	_entriesDelete = new AnalysisEntries(
	{
		new AnalysisEntry([&](){ emit this->dataRemoveColumn();			},					"delete-column",				[&](){ return fq(tr("Delete column"));},								true,		"menu-column-remove"),
		new AnalysisEntry([&](){ emit this->dataRemoveRow();			},					"delete-row",					[&](){ return fq(tr("Delete row"));},									true,		"menu-row-remove"),
		new AnalysisEntry([&](){ emit this->cellsClear();				},					"clear-cells",					[&](){ return fq(tr("Clear cells"));},									true,		"menu-cells-clear")
	});

	_entriesSynchOn = new AnalysisEntries(
	{
		new AnalysisEntry([&]() { MainWindow::singleton()->startDataEditorHandler(); },		"open-datafile",				[](){ return fq(tr("Open datafile with default spreadsheet editor"));},	true, ""),
		new AnalysisEntry([&]() { emit setDataSynchronisation(false);	},					"stop-externaledit",			[](){ return fq(tr("Turn external data synchronisation off"));},		true, "")
	});

	if(QCoreApplication::applicationName() == "JASPTest")
		_testButton			= new RibbonButton(this, "Test",					[&](){ return fq(tr("Test"));},						"summarize.svg",			false, [&](){ emit runTests();	},									[&](){return tr("Run tests");},													true);
	_analysesButton			= new RibbonButton(this, "Analyses",				[&](){ return fq(tr("Analyses"));},					"JASP_logo_green.svg",		false, [&](){ emit finishCurrentEdit(); emit showStatistics(); },	[&](){return tr("Switch JASP to analyses mode");},								true);
	_dataSwitchButton		= new RibbonButton(this, "Data",					[&](){ return fq(tr("Edit Data"));},				"data-button.svg",			false, [&](){ emit showData(); },									[&](){return tr("Switch JASP to data editing mode");},							false, false, false);
	_dataNewButton			= new RibbonButton(this, "Data-New",				[&](){ return fq(tr("New Data"));},					"data-button-new.svg",		false, [&](){ emit showNewData();	 },								[&](){return tr("Open a workspace without data");},								true, false, false);
	_dataInsertButton		= new RibbonButton(this, "Data-New-Insert",			[&](){ return fq(tr("New Data"));},					"data-button-new.svg",		false,  [&](){ emit addNewDataSet();	 },								[&](){return tr("Open a workspace without data");},								true, false, false);
	_dataResizeButton		= new RibbonButton(this, "Data-Resize",				[&](){ return fq(tr("Resize Data"));},				"data-button-resize.svg",	false, [&](){ emit resizeData(); },									[&](){return tr("Resize your dataset");},										false);
	_insertButton			= new RibbonButton(this, "Data-Insert",				[&](){ return fq(tr("Insert"));},					"data-button-insert.svg",	_entriesInsert,														[&](){return tr("Insert empty columns or rows");});
	_removeButton			= new RibbonButton(this, "Data-Remove",				[&](){ return fq(tr("Remove"));},					"data-button-erase.svg",	_entriesDelete,														[&](){return tr("Remove columns or rows");});
	_synchroniseOnButton	= new RibbonButton(this, "Data-Synch-On",			[&](){ return fq(tr("Synchronisation"));},			"data-button-sync-off.svg",	true, [&](){ emit setDataSynchronisation(true); },					[&](){return tr("Turn external data synchronisation on");},						false);
	_synchroniseOffButton	= new RibbonButton(this, "Data-Synch-Off",			[&](){ return fq(tr("Synchronisation"));},			"data-button-sync-on.svg",	_entriesSynchOn,													[&](){return tr("Turn external data synchronisation off");},					true);
	_undoButton				= new RibbonButton(this, "Data-Undo",				[&](){ return fq(tr("Undo"));},						"menu-undo.svg",			true,  [&](){ emit dataUndo(); },									[&](){return tr("Undo changes, %1+Z").arg(getShortCutKey());},					true, false, false);
	_redoButton				= new RibbonButton(this, "Data-Redo",				[&](){ return fq(tr("Redo"));},						"menu-redo.svg",			true,  [&](){ emit dataRedo(); },									[&](){return tr("Redo changes, %1+shift+Z or %1+Y").arg(getShortCutKey());},	true, false, false);


	_dataNewButton->setActive(true);
	_dataInsertButton->setActive(true);
	connect(this, &RibbonModel::dataLoadedChanged,							_dataSwitchButton,	[=](bool loaded)			{ _dataSwitchButton	->setEnabled(loaded);				});
	connect(this, &RibbonModel::dataLoadedChanged,							_dataNewButton,		[=](bool loaded)			{ _dataNewButton	->setEnabled(!loaded);				});
	connect(MainWindow::singleton(), &MainWindow::dataAvailableChanged,		_dataSwitchButton,	[=](bool dataAvailable)		{ _dataSwitchButton	->setActive(dataAvailable);			});

	connect(this, &RibbonModel::dataLoadedChanged,		_insertButton,			&RibbonButton::setEnabled);
	connect(this, &RibbonModel::dataLoadedChanged,		_removeButton,			&RibbonButton::setEnabled);
	connect(this, &RibbonModel::dataLoadedChanged,		_dataResizeButton,		&RibbonButton::setEnabled);
	
	connect(this, &RibbonModel::synchronisationChanged, _synchroniseOnButton,	[=](bool synching){ _synchroniseOnButton->setEnabled(!synching); });
	connect(this, &RibbonModel::synchronisationChanged, _synchroniseOffButton,	[=](bool synching){ _synchroniseOffButton->setEnabled(synching); });

	{
		DataSetView * view = DataSetView::mainDataViewer();

		auto setUnAndRedoButtonLambda = [&,view]()
		{
			_undoButton->setActive(!view->undoText().isEmpty());
			_redoButton->setActive(!view->redoText().isEmpty());

			_undoButton->setToolTipF([&,view](){return tr("Undo %2 (%1+Z)")					.arg(getShortCutKey()).arg(view->undoText());});
			_redoButton->setToolTipF([&,view](){return tr("Redo %2 (%1+shift+Z or %1+Y)")	.arg(getShortCutKey()).arg(view->redoText());});
		};



		connect(view, &DataSetView::undoChanged,			setUnAndRedoButtonLambda);
		connect(this, &RibbonModel::dataLoadedChanged,		setUnAndRedoButtonLambda);
	}

	if(_testButton)
		addRibbonButtonModel(_testButton,			size_t(RowType::Analyses));
	addRibbonButtonModel(_dataSwitchButton,			size_t(RowType::Analyses));
	addRibbonButtonModel(_dataNewButton,			size_t(RowType::Analyses));
	addRibbonButtonModel(new RibbonButton(this),	size_t(RowType::Analyses));

	addRibbonButtonModel(_analysesButton,			size_t(RowType::Data));
	addRibbonButtonModel(new RibbonButton(this),	size_t(RowType::Data));
	addRibbonButtonModel(_synchroniseOnButton,		size_t(RowType::Data));
	addRibbonButtonModel(_synchroniseOffButton,		size_t(RowType::Data));
	addRibbonButtonModel(_dataInsertButton,			size_t(RowType::Data));
	addRibbonButtonModel(_dataResizeButton,			size_t(RowType::Data));
	addRibbonButtonModel(_insertButton,				size_t(RowType::Data));
	addRibbonButtonModel(_removeButton,				size_t(RowType::Data));
	addRibbonButtonModel(_undoButton,				size_t(RowType::Data));
	addRibbonButtonModel(_redoButton,				size_t(RowType::Data));
}

void RibbonModel::addSpecialRibbonButtonsLate()
{
	addRibbonButtonModel(new RibbonButton(this, "R", [&](){return fq(tr("R console")); }, "Rlogo.svg", false, [&](){ emit showRCommander(); }, [&](){ return tr("Execute R code in a console");}, false, true), size_t(RowType::Analyses));
}

void RibbonModel::setDataMode(bool data)
{
	if(data)	showData();
	else		showStatistics();
}

void RibbonModel::dynamicModuleChanged(Modules::DynamicModule * dynMod)
{
	Log::log() << "void RibbonModel::dynamicModuleChanged(" << dynMod->toString() << ")" << std::endl;

	for(const auto & nameButton : _buttonModelsByName)
		if(nameButton.second->module() == dynMod)
			nameButton.second->reloadDynamicModule(dynMod);
}

void RibbonModel::addRibbonButtonModel(RibbonButton* model, size_t row)
{
	if(isModuleName(model->name()))
		removeRibbonButtonModel(model->name());

	if(_currentRow == row)
		beginInsertRows(QModelIndex(), rowCount(), rowCount());

	_buttonNames[row].push_back(model->name());
	_buttonModelsByName[model->name()] = model;

	if(_currentRow == row)
		endInsertRows();

	connect(model, &RibbonButton::iChanged,				this, &RibbonModel::ribbonButtonModelChanged);
}

void RibbonModel::dynamicModuleReplaced(Modules::DynamicModule * oldModule, Modules::DynamicModule * module)
{
	for(const auto & nameButton : _buttonModelsByName)
		if(nameButton.second->module() == oldModule || nameButton.first == oldModule->name())
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
	case ModuleNameRole:	return ribbonButtonModelAt(row)->nameQ();
	case ModuleRole:		return QVariant::fromValue(ribbonButtonModelAt(row)->module());
	case BundledRole:		return ribbonButtonModelAt(row)->isBundled();
	case DevModRole:		return ribbonButtonModelAt(row)->module() && ribbonButtonModelAt(row)->module()->isDevMod();
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
		{ DevModRole,		"isDevMod"			},
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

    for(size_t row=0; row <= size_t(RowType::Data); row++)
	{
		int indexRemoved = -1;

        for(int i=_buttonNames[row].size() - 1; i >= 0; i--) {
			if(_buttonNames[row][i] == moduleName)
			{
				indexRemoved = i;
				break;
			}
        }

		if(indexRemoved != -1)
		{
			if(row == _currentRow)
				beginRemoveRows(QModelIndex(), indexRemoved, indexRemoved);

			delete _buttonModelsByName[moduleName];
			_buttonModelsByName.erase(moduleName);

			_buttonNames[row].erase(_buttonNames[row].begin() + indexRemoved);

			if(row == _currentRow)
				endRemoveRows();
		}
	}
}

QString RibbonModel::moduleName(size_t index) const	
{ 
	return QString::fromStdString(_buttonNames[_currentRow][index]);
}

RibbonButton *RibbonModel::ribbonButtonModelAt(size_t index) const	
{ 
	return ribbonButtonModel(		_buttonNames[_currentRow][index]); 
}

void RibbonModel::analysisClicked(QString analysisFunction, QString analysisQML, QString analysisTitle, QString module)
{
	RibbonButton * button = ribbonButtonModel(fq(module));
	
	if(button->isSpecial())		button->runSpecial(analysisFunction);
	else						emit analysisClickedSignal(analysisFunction, analysisQML, analysisTitle, module);
}

void RibbonModel::setCurrentRow(int currentRow)
{
	size_t cur = currentRow;
	if (_currentRow == cur)
		return;

	beginResetModel();
	_currentRow = cur;
	endResetModel();

	emit currentRowChanged();
	emit dataModeChanged(dataMode());
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
		emit invalidateFilterModel();
	}
}

QStringList RibbonModel::getModulesEnabled() const
{
	QStringList list;
	
	for(auto & nameButton : _buttonModelsByName)
		if(nameButton.second->remember() && nameButton.second->enabled()) //Only selection-participants (modules and R-console) belong in the remembered selection
			list.append(nameButton.second->nameQ());
	
	return list;
}


Modules::AnalysisEntry *RibbonModel::getAnalysis(std::string moduleName, const std::string & analysisName)
{
	if(moduleName == "Common")
		moduleName = getModuleNameFromAnalysisName(analysisName);

	RibbonButton			* ribbonButton	= ribbonButtonModel(moduleName);

	if (ribbonButton)
		return ribbonButton->getEntry(analysisName);
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
	for (const std::string & moduleName : _buttonNames[size_t(RowType::Analyses)])
		for (const std::string & name : _buttonModelsByName[moduleName]->getAllEntries())
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
			for(size_t i=0; i<_buttonNames[_currentRow].size(); i++)
				if(_buttonNames[_currentRow][i] == keyval.first)
					return int(i);
	return -1;
}

QStringList RibbonModel::getModuleOrder() const
{
	QStringList order;

	for(const auto & name : _buttonNames[size_t(RowType::Analyses)])
	{
		RibbonButton * button = ribbonButtonModel(name);
		if(button && button->module())
			order.append(tq(name));
	}

	return order;
}

void RibbonModel::setModuleOrder(QStringList order)
{
	stringvec & rowNames = _buttonNames[size_t(RowType::Analyses)];

	//Collect the anchored prefix (Test/Data/New-data/separator), the current module segment and the anchored suffix (R-console).
	//The rebuild also normalizes the layout: modules become contiguous, so a module appended after the R-console (runtime install) slots back into the segment.
	stringvec	prefix,
				currentModules,
				suffix;
	bool seenModule = false;

	for(const auto & name : rowNames)
	{
		RibbonButton * button = ribbonButtonModel(name);
		if(button && button->module())
		{
			seenModule = true;
			currentModules.push_back(name);
		}
		else if(seenModule)	suffix.push_back(name);
		else				prefix.push_back(name);
	}

	std::set<std::string>	current(currentModules.begin(), currentModules.end()),
							ordered;
	stringvec			newModules;

	for(const QString & qName : order)
	{
		std::string name = fq(qName);
		if(current.count(name) && !ordered.count(name))
		{
			ordered.insert(name);
			newModules.push_back(name);
		}
	}

	for(const auto & name : currentModules) //Modules not mentioned (newly installed, or first run) are appended in their current relative order
		if(!ordered.count(name))
			newModules.push_back(name);

	if(newModules == currentModules)
		return;

	beginResetModel();

	rowNames = prefix;
	rowNames.insert(rowNames.end(), newModules.begin(), newModules.end());
	rowNames.insert(rowNames.end(), suffix.begin(), suffix.end());

	endResetModel();
}

void RibbonModel::moveModule(int from, int to)
{
	if(_currentRow != size_t(RowType::Analyses) || from == to || from < 0 || to < 0)
		return;

	stringvec & rowNames = _buttonNames[size_t(RowType::Analyses)];

	if(size_t(from) >= rowNames.size() || size_t(to) >= rowNames.size())
		return;

	RibbonButton *	fromButton	= ribbonButtonModel(rowNames[size_t(from)]),
				*	toButton	= ribbonButtonModel(rowNames[size_t(to)]);

	//Only actual modules can be reordered; special buttons (Data, separator, R-console, ...) stay anchored
	if(!fromButton || !fromButton->module() || !toButton || !toButton->module())
		return;

	int destination = to > from ? to + 1 : to;

	if(!beginMoveRows(QModelIndex(), from, from, QModelIndex(), destination))
		return;

	std::string moved = rowNames[size_t(from)];
	rowNames.erase(rowNames.begin() + from);
	rowNames.insert(rowNames.begin() + to, moved);

	endMoveRows();

	Settings::setValue(Settings::MODULES_ORDER, getModuleOrder().join('|'));
}

void RibbonModel::ribbonButtonModelChanged(RibbonButton* model)
{
	int row = ribbonButtonModelIndex(model);
	if(row > -1)
		emit dataChanged(index(row), index(row));
}

void RibbonModel::refresh()
{
	beginResetModel();
	endResetModel();	
}
