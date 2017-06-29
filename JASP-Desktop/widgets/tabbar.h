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
#include <QSettings>

class TabBar : public QWidget
{
	Q_OBJECT
public:
	explicit TabBar(QWidget *parent = 0);

	void addTab(QString tabName);
	void removeTab(QString tabName);
	void removeTab(int index);
	QString getCurrentActiveTab();
	void setExactPValues(bool exactPValues);

	void addOptionsTab();
	void addHelpTab();

	int count() const;


signals:
	void currentChanged(int index);
	void helpToggled(bool on);
	void dataAutoSynchronizationChanged(bool on);
	void setExactPValuesHandler(bool exactPValues);

public slots:
	void setCurrentIndex(int index);

private slots:
	void tabSelectedHandler();
	void helpToggledHandler(bool on);
	void showAbout();
	void showPreferences();
	void toggleHelp();
	void toggleSEM();
	void toggleReinforcement();
	void toggleSummaryStats();
	void toggleMachineLearning();

private:

	QWidget *_background;
	QList<QPushButton *> _tabButtons;
	QGridLayout *_backgroundLayout;
	QHBoxLayout *_layout;

	QPushButton *_helpTab;
	QComboBox *_comboTab;
	QMenu *_menuTab;
	QMenuBar *_menuBarTab;
	QSettings _settings;
	QString _currentActiveTab;
};

#endif // TABBAR_H
