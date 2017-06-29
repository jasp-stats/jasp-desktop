//
// Copyright (C) 2013-2017 University of Amsterdam
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
#include "aboutdialog.h"
#include "preferencesdialog.h"
#include "widgets/ribbonbutton.h"

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
}

void TabBar::addTab(QString tabName)
{
	foreach (QPushButton *button, _tabButtons)
	{
		if (button->objectName() == tabName)
			return;
	}

	QPushButton *button = new QPushButton(tabName, this);
	button->setStyleSheet("border-top-left-radius:4px;border-top-right-radius:4px;");
	button->setObjectName(tabName);
	button->setCheckable(true);
	connect(button, SIGNAL(clicked()), this, SLOT(tabSelectedHandler()));

	if (_tabButtons.size() == 0)
		button->setObjectName("first"); //just to give it the proper (blue) stylesheet

	_layout->insertWidget(_tabButtons.size(), button);
	_tabButtons.append(button);
    button->clicked();
}

void TabBar::removeTab(int index)
{
	QPushButton *button = _tabButtons.at(index);
	_tabButtons.removeAt(index);
	delete button;
}

void TabBar::removeTab(QString tabName)
{
    QPushButton *lastbutton;
	foreach (QPushButton *button, _tabButtons)
	{
		if (button->objectName() == tabName)
		{
			_tabButtons.removeAll(button);
			delete button;
            if (lastbutton) lastbutton->clicked();

			return;
		}
        lastbutton = button;
	}
}


void TabBar::addHelpTab()
{

	RibbonButton *rb = new RibbonButton();
	rb->setIcon(QIcon(":/icons/summarize.svg"));
	rb->setPopupMode(QToolButton::InstantPopup);
	rb->setProperty("button-type", "summarize");
	rb->setMinimumSize(30,0);
	_layout->setContentsMargins(0,0,2,0);

	QMenu  *helpmenu   = new QMenu(this);

	QAction *act_about = new QAction("About",helpmenu);
	QAction *act_extrahelp = new QAction("Help",helpmenu);
	QAction *act_preferences = new QAction("Preferences",helpmenu);

	// About
	act_about->setObjectName("About");
	helpmenu->addAction(act_about);
	helpmenu->addSeparator();

	//Special Help
	act_extrahelp->setObjectName("Special Help");
	act_extrahelp->setCheckable(true);
	act_extrahelp->setChecked(false);
	helpmenu->addAction(act_extrahelp);
	helpmenu->addSeparator();

	// Preferences
	act_preferences->setObjectName("Preferences");
	helpmenu->addAction(act_preferences);
	helpmenu->addSeparator();

	//Options
	QMenu *optionmenu   = new QMenu("Modules",this);
	QAction *sem = new QAction("SEM",optionmenu);
	QAction *rei = new QAction("Reinforcement Learning",optionmenu);
	QAction *summaryStats = new QAction("Summary Stats",optionmenu);
	QAction *machineLearning = new QAction("Machine Learning", optionmenu);

	//SEM
	QVariant sem_setting = _settings.value("plugins/sem", false);
	sem->setObjectName("SEM");
	sem->setCheckable(true);
	sem->setChecked(sem_setting.canConvert(QVariant::Bool) && sem_setting.toBool());
	optionmenu->addAction(sem);

	//Reinforcement
	QVariant ri_setting = _settings.value("toolboxes/r11tLearn", false);
	rei->setObjectName("Reinforcement Learning");
	rei->setCheckable(true);
	rei->setChecked(ri_setting.canConvert(QVariant::Bool) && ri_setting.toBool());

	//Summary Stats
	QVariant sumStats_setting = _settings.value("toolboxes/summaryStatistics", false);
	summaryStats->setObjectName("Summary Stats");
	summaryStats->setCheckable(true);
	summaryStats->setChecked(sumStats_setting.canConvert(QVariant::Bool) && sumStats_setting.toBool());
	optionmenu->addAction(summaryStats);

	//Machine Learning
	QVariant machineLearning_setting = _settings.value("toolboxes/machineLearning", false);
	machineLearning->setObjectName("Machine Learning");
	machineLearning->setCheckable(true);
	machineLearning->setChecked(machineLearning_setting.canConvert(QVariant::Bool) && machineLearning_setting.toBool());
	optionmenu->addAction(machineLearning);

#ifdef QT_DEBUG
	optionmenu->addAction(rei);
#endif

	optionmenu->acceptDrops();
	helpmenu->acceptDrops();
	helpmenu->addMenu(optionmenu);

	rb->setMenu(helpmenu);
	_layout->addWidget(rb);

	//Slots helpmenu
	connect(act_about, SIGNAL(triggered()), this, SLOT(showAbout()));
	connect(act_preferences, SIGNAL(triggered()), this, SLOT(showPreferences()));
	connect(act_extrahelp, SIGNAL(triggered()), this, SLOT(toggleHelp()));

	// Slots options
	connect(sem, SIGNAL(triggered()), this, SLOT(toggleSEM()));
	connect(rei, SIGNAL(triggered()), this, SLOT(toggleReinforcement()));
	connect(summaryStats, SIGNAL(triggered()), this, SLOT(toggleSummaryStats()));
	connect(machineLearning, SIGNAL(triggered()), this, SLOT(toggleMachineLearning()));
}

void TabBar::showAbout()
{
	AboutDialog aboutdialog;
	aboutdialog.setModal(true);
	aboutdialog.exec();
}

void TabBar::showPreferences()
{
	PreferencesDialog preferencesdialog(this);
	preferencesdialog.setModal(true);
	preferencesdialog.exec();
}

void TabBar::toggleHelp()
{
	static bool on;
	on = ! on;
	helpToggledHandler(on);
}

void TabBar::toggleSEM()
{
	QVariant sem_setting = _settings.value("plugins/sem", false);
	static bool on = (sem_setting.canConvert(QVariant::Bool) && sem_setting.toBool());
	on = ! on;
	if (on)
		this->addTab("SEM");
	else
		this->removeTab("SEM");
}

void TabBar::toggleReinforcement()
{
	QVariant ri_setting = _settings.value("toolboxes/r11tLearn", false);
	static bool on = (ri_setting.canConvert(QVariant::Bool) && ri_setting.toBool());
	on = ! on;
	if (on)
		this->addTab("R11t Learn");
	else
		this->removeTab("R11t Learn");
}

void TabBar::toggleSummaryStats()
{
	QVariant sumStats_setting = _settings.value("toolboxes/summaryStatistics", false);
	static bool on = (sumStats_setting.canConvert(QVariant::Bool) && sumStats_setting.toBool());
	on = ! on;
    if (on)
    {
		this->addTab("Summary Stats");
    }
	else
    {
		this->removeTab("Summary Stats");
    }
}

void TabBar::toggleMachineLearning()
{
	QVariant machineLearning_setting = _settings.value("toolboxes/machineLearning", false);
	static bool on = (machineLearning_setting.canConvert(QVariant::Bool) && machineLearning_setting.toBool());
	on = !on;
	if (on)
	{
		this->addTab("Machine Learning");
	}
	else
	{
		this->removeTab("Machine Learning");
	}
}

int TabBar::count() const
{
	return _tabButtons.length();
}

void TabBar::setCurrentIndex(int index)
{
	int i = 0;

	foreach (QPushButton *button, _tabButtons)
	{
		button->setChecked(i == index);
        if (i == index) _currentActiveTab = button->objectName();
		i++;
	}

	emit currentChanged(index);
}

void TabBar::setExactPValues(bool exactPValues)
{
	emit setExactPValuesHandler(exactPValues);
}

void TabBar::tabSelectedHandler()
{
	QObject *source = this->sender();
	int i = 0;

	foreach (QPushButton *button, _tabButtons)
	{
		if (source == button)
		{
			_currentActiveTab = button->objectName();
			setCurrentIndex(i);
			return;
		}
		i++;
	}
}

QString TabBar::getCurrentActiveTab()
{
	return _currentActiveTab;
}

void TabBar::helpToggledHandler(bool on)
{
	emit helpToggled(on);
}
