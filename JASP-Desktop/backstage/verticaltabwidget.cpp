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

#include "verticaltabwidget.h"

VerticalTabWidget::VerticalTabWidget(QWidget *parent) : QWidget(parent)
{
	_layout = new QHBoxLayout(this);
	_layout->setContentsMargins(0, 0, 0, 0);
	_layout->setSpacing(0);

	_tabBar = new VerticalTabBar(this);
	_stackWidget = new QStackedWidget(this);
	_stackWidget->layout()->setContentsMargins(0, 0, 0, 0);

	_layout->addWidget(_tabBar);
	_layout->addWidget(_stackWidget, 1);

	connect(_tabBar, SIGNAL(currentChanged(int)), _stackWidget, SLOT(setCurrentIndex(int)));
}

void VerticalTabWidget::addTab(QWidget *page, const QString &label, const QIcon &icon)
{
	_tabBar->addTab(label, icon);
	_stackWidget->addWidget(page);
}

void VerticalTabWidget::showTab(QWidget *page)
{
	int index = _stackWidget->indexOf(page);
	_tabBar->showTab(index);
}

void VerticalTabWidget::hideTab(QWidget *page)
{
	int index = _stackWidget->indexOf(page);
	_tabBar->hideTab(index);
}

void VerticalTabWidget::setTabEnabled(int index, bool enable)
{
	_stackWidget->widget(index)->setEnabled(enable);
	_tabBar->setTabEnabled(index, enable);
}

VerticalTabBar *VerticalTabWidget::tabBar()
{
	return _tabBar;
}

QWidget *VerticalTabWidget::container()
{
	return _stackWidget;
}

