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

#ifndef TABBAR_H
#define TABBAR_H

#include <QWidget>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QList>
#include <QPushButton>
#include <QComboBox>
#include <QMenu>
#include <QMenuBar>
#include <QAction>
#include <QSignalMapper>

class PreferencesDialog;
class DynamicModules;

#include "gui/aboutdialog.h"
#include "modules/ribbonmodel.h"

class TabBar : public QWidget
{
	Q_OBJECT

public:
	explicit TabBar(QWidget *parent = 0);
	void init(DynamicModules * dynamicModules, RibbonModel * ribbonModel);

	void addTab(QString name);
	void addModulesPlusButton();
	void removeTab(QString tabName);

	QString getCurrentActiveTab();
	void setCurrentModuleActive();
	void setCurrentTab(QString name);
	QStringList getCurrentModules();
	void setModulePlusMenu(QStringList usedModules = QStringList());
	void addModuleInstallerEntryToPlusMenu();

	void setExactPValues(bool exactPValues);
    void setFixDecimals(QString numDecimals);
    void emptyValuesChanged();



	int count() const;
	PreferencesDialog *getPreferencesDialog();

signals:
	void currentChanged(int index);
	void helpToggled(bool on);
	void dataAutoSynchronizationChanged(bool on);
	void setExactPValuesHandler(bool exactPValues);
	void setFixDecimalsHandler(QString numDecimals);
	void emptyValuesChangedHandler();

private slots:
	void tabSelectedHandler();
	void helpToggledHandler(bool on);
	void showAbout();
	void showPreferences();
	void toggleHelp();
	void toggleModule(QString name);
	void handleModuleButton();

private:
	QWidget *_background;
	QList<QPushButton *> _tabButtons;
	QGridLayout *_backgroundLayout;
	QHBoxLayout *_layout;

	QPushButton *_helpTab;
	QPushButton *_currentTab = NULL;
	QPushButton *_currentModule = NULL;

	AboutDialog *_aboutDialog;
	PreferencesDialog *_preferencesDialog;
	QPushButton *_modulesButton;
	QSignalMapper *_signalModulesMapper;

	DynamicModules * _dynamicModules;
	RibbonModel * _ribbonModel;

};

#endif // TABBAR_H
