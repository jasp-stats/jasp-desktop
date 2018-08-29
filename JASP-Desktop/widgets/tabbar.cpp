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

#include "tabbar.h"
#include <QMessageBox>
#include <QQuickWidget>
#include <QQmlContext>

#include "gui/preferencesdialog.h"
#include "widgets/ribbonbutton.h"
#include "modules/dynamicmodules.h"
#include "modules/dynamicmodule.h"

using namespace std;


TabBar::TabBar(QWidget *parent) :
	QWidget(parent)
{
	_helpTab = NULL;

	_background = new QWidget(this);
	_background->setObjectName("background");

	_backgroundLayout = new QGridLayout(this);
	_backgroundLayout->setMargin(0);
	_backgroundLayout->addWidget(_background);

	_layout = new QHBoxLayout(_background);
	_layout->setMargin(0);
	_background->setLayout(_layout);

	setLayout(_backgroundLayout);

	_layout->addStretch(1);

	_aboutDialog = new AboutDialog(this);
	_preferencesDialog = new PreferencesDialog(this);
	_modulesButton = NULL;
}

QStringList TabBar::getCurrentModules()
{
	QStringList result;
	for (QPushButton *button : _tabButtons)
	{
		QString name = button->objectName();
		result.append(name);
	}

	return result;
}


void TabBar::addModulesPlusButton()
{
	QPushButton *button = new QPushButton("", this);
	button->setToolTip("Load module");
	button->setIcon(QIcon(":/icons/addition.png"));
	_modulesButton = button;
	_signalModulesMapper = new QSignalMapper(this);
	connect(_modulesButton, &QPushButton::pressed, this, &TabBar::handleModuleButton);

	QStringList currentModules = getCurrentModules();

	//Modules menu
	QMenu *modulesmenu   = new QMenu(this);

	for (auto i : _ribbonModel->moduleNames()) {

		QAction *action = new QAction(QString::fromStdString(i), modulesmenu);
		QString name = QString::fromStdString(i);
		action->setObjectName(name);
		action->setCheckable(true);
		action->setChecked(currentModules.indexOf(name) >= 0);
		modulesmenu->addAction(action);
		_signalModulesMapper->setMapping(action, name);
		connect(action, &QAction::triggered, _signalModulesMapper, QOverload<>::of(&QSignalMapper::map));
	}

	connect(_signalModulesMapper, QOverload<const QString &>::of(&QSignalMapper::mapped), this, &TabBar::toggleModule);
	modulesmenu->acceptDrops();
	button->setMenu(modulesmenu);
	_layout->insertWidget(_tabButtons.size(), button);

	button->setStyleSheet("border-top-left-radius:4px;border-top-right-radius:4px;");
	button->setObjectName("Modules");
	button->setCheckable(true);
	connect(button, &QPushButton::clicked, this, &TabBar::tabSelectedHandler);

	_tabButtons.append(button);
}


void TabBar::addModuleInstallerEntryToPlusMenu()
{
	QString name	= "Install Module";
	QAction *action = new QAction(name,  _modulesButton->menu());

	action->setObjectName(name);
	action->setCheckable(false);
	_modulesButton->menu()->addAction(action);

	connect(action, &QAction::triggered, _dynamicModules, &DynamicModules::openModuleInstallerWindow);
}


void TabBar::addTab(QString name)
{
	for (QPushButton *button : _tabButtons) {

		if (button->objectName() == name)
			return;
	}

	QPushButton *button = new QPushButton(name, this);

	if (_ribbonModel->isModuleName(name.toStdString())) {

		button->setText(name);
		_layout->insertWidget(_tabButtons.size() - 1, button);
		_currentModule = button;
	} else {
		_layout->insertWidget(_tabButtons.size(), button);
	}

	button->setStyleSheet("border-top-left-radius:4px;border-top-right-radius:4px;");
	if (_tabButtons.size() == 0)
		button->setObjectName("first"); //just to give it the proper (blue) stylesheet
	else
		button->setObjectName(name);

	button->setCheckable(true);
	connect(button, &QPushButton::clicked, this, &TabBar::tabSelectedHandler);

	_tabButtons.append(button);
	button->clicked();
}


void TabBar::removeTab(QString tabName)
{
	QPushButton *lastButton = NULL;
	// Init lastButton in case with first button.
	for(QPushButton *button : _tabButtons)
		if (button->objectName() == "first")
			lastButton = button;

	for (QPushButton *button : _tabButtons)
	{
		QString buttonName = button->objectName();
		if (buttonName == tabName)
		{
			if (lastButton->objectName() == "first")
			{
				// Check whether another available module exists
				for (QPushButton *button2 : _tabButtons)
					if (button2 != button &&  _ribbonModel->isModuleName(button2->objectName().toStdString()))
						lastButton = button2;
			}
			if (button == _currentTab)
			{
				_currentTab = lastButton;
				_currentTab->clicked();
			}
			if (button == _currentModule)
			{
				if (_ribbonModel->isModuleName(lastButton->objectName().toStdString()))
					_currentModule = lastButton;
				else
					_currentModule = NULL;
			}
			_tabButtons.removeAll(button);
			delete button;

			return;
		}
		if (buttonName != "Modules")
			lastButton = button;
	}
}


void TabBar::init(DynamicModules * dynamicModules, RibbonModel * ribbonModel)
{
	_dynamicModules = dynamicModules;
	_ribbonModel    = ribbonModel;

	setFocusPolicy(Qt::NoFocus);
	addTab("File");
	addModulesPlusButton();
	addTab("Common"); // Common module must be added after the Plus Button,in order to set it at the right place
	setModulePlusMenu(); // A a check to Common in the menu

	RibbonButton *rb = new RibbonButton();
	rb->setIcon(QIcon(":/icons/summarize.svg"));
	rb->setPopupMode(QToolButton::InstantPopup);
	rb->setProperty("button-type", "summarize");
	rb->setMinimumSize(30,0);
	rb->setToolTip("Options");
	_layout->setContentsMargins(0,0,2,0);

	QMenu  *optionsmenu			= new QMenu(this);

	QAction *act_about			= new QAction("About",optionsmenu);
	QAction *act_extrahelp		= new QAction("Help",optionsmenu);
	QAction *act_preferences	= new QAction("Preferences",optionsmenu);

	// About
	act_about->setObjectName("About");
	optionsmenu->addAction(act_about);
	optionsmenu->addSeparator();

	//Special Help
	act_extrahelp->setObjectName("Special Help");
	act_extrahelp->setCheckable(true);
	act_extrahelp->setChecked(false);
	optionsmenu->addAction(act_extrahelp);
	optionsmenu->addSeparator();

	// Preferences
	act_preferences->setObjectName("Preferences");
	optionsmenu->addAction(act_preferences);
	optionsmenu->addSeparator();

	optionsmenu->acceptDrops();

	rb->setMenu(optionsmenu);
	_layout->addWidget(rb);

	//Slots preferences
	connect(act_about,			&QAction::triggered, this, &TabBar::showAbout);
	connect(act_preferences,	&QAction::triggered, this, &TabBar::showPreferences);
	connect(act_extrahelp,		&QAction::triggered, this, &TabBar::toggleHelp);

}


void TabBar::showAbout()
{
	_aboutDialog->show();
	_aboutDialog->raise();
	_aboutDialog->activateWindow();
	// The last function performs the same operation as clicking the mouse on the title bar
	// If you want to ensure that the window is stacked on top as well you should also call raise(). 	//Note that the window must be visible, otherwise activateWindow() has no effect.
}


void TabBar::showPreferences()
{
	_preferencesDialog->show();
	_preferencesDialog->raise();
	_preferencesDialog->activateWindow();
}


void TabBar::toggleHelp()
{
	static bool on;
	on = ! on;
	helpToggledHandler(on);
}

void TabBar::setModulePlusMenu(QStringList usedModules)
{
	QStringList currentModules = getCurrentModules();
	for (QAction *action : _modulesButton->menu()->actions())
	{
		QString moduleName = action->objectName();
		if (usedModules.contains(moduleName))
		{
			if (!action->isChecked())
				action->setChecked(true);
			if (!currentModules.contains(moduleName))
				addTab(moduleName);
			action->setEnabled(false);
		}
		else
		{
			action->setEnabled(true);
			action->setChecked(currentModules.contains(moduleName));
		}
	}
}

void TabBar::toggleModule(QString name)
{
	bool on = true;
	for (QAction *action : _modulesButton->menu()->actions())
		if (action->objectName() == name)
			on = action->isChecked();

	if (on)	addTab(name);
	else	removeTab(name);
}

void TabBar::handleModuleButton()
{
	if (_modulesButton != NULL)
		_modulesButton->setAttribute(Qt::WA_UnderMouse, false);
}


int TabBar::count() const
{
	return _tabButtons.length();
}


PreferencesDialog *TabBar::getPreferencesDialog()
{
	return _preferencesDialog;
}

void TabBar::setCurrentModuleActive()
{
	if (_currentModule)
		setCurrentTab(_currentModule->objectName());
}


void TabBar::setExactPValues(bool exactPValues)
{
	emit setExactPValuesHandler(exactPValues);
}


void TabBar::setFixDecimals(QString numDecimals)
{
	emit setFixDecimalsHandler(numDecimals);
}


void TabBar::emptyValuesChanged()
{
	emit emptyValuesChangedHandler();
}


void TabBar::setCurrentTab(QString name)
{
	int i = 0, index = -1;

	for (QPushButton *button : _tabButtons)
	{
		if (button->objectName() == name)
		{
			button->setChecked(true);
			index = i;
			_currentTab = button;
			if (_ribbonModel->isModuleName(name.toStdString()))
				_currentModule = button;
		}
		else
			button->setChecked(false);

		i++;
	}

	if (index >= 0)
		emit currentChanged(index);
}

void TabBar::tabSelectedHandler()
{
	QObject *source = this->sender();
	setCurrentTab(source->objectName());
}

QString TabBar::getCurrentActiveTab()
{
	return _currentTab ? _currentTab->objectName() : QString();
}


void TabBar::helpToggledHandler(bool on)
{
	emit helpToggled(on);
}
