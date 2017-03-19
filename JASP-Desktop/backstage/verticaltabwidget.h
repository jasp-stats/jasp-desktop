//
// Copyright (C) 2017 University of Amsterdam
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

#ifndef VERTICALTABWIDGET_H
#define VERTICALTABWIDGET_H

#include <QStackedWidget>
#include <QHBoxLayout>

#include "verticaltabbar.h"

class VerticalTabWidget : public QWidget
{
public:
	VerticalTabWidget(QWidget *parent = NULL);

	void addTab(QWidget *page, const QString &label, const QIcon &icon = QIcon());
	void hideTab(QWidget *page);
	void showTab(QWidget *page);
	void setTabEnabled(int index, bool enable);

	void setTabStyleSheet(const QString &styleSheet);

	VerticalTabBar *tabBar();
	QWidget *container();

private:
	QHBoxLayout *_layout;
	VerticalTabBar *_tabBar;
	QStackedWidget *_stackWidget;
};

#endif // VERTICALTABWIDGET_H
