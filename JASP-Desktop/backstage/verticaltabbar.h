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

#ifndef VERTICALTABBAR_H
#define VERTICALTABBAR_H

#include <QWidget>
#include <QVBoxLayout>
#include <QIcon>
#include <QButtonGroup>
#include <QPushButton>

#include "common.h"

class VerticalTabBar : public QWidget
{
	Q_OBJECT
public:
	explicit VerticalTabBar(QWidget *parent = 0);

	void addTab(const QString &label, const QIcon &icon = QIcon());
	void hideTab(int index);
	void showTab(int index);
	bool isTabVisible(int index);
	void setTabEnabled(int index, bool enable);

	void setCurrentIndex(int index);
	void click(int index);
	int currentIndex() const;
	int count() const;

protected:
	void paintEvent(QPaintEvent *) OVERRIDE;

signals:
	void currentChanging(int newIndex, bool &cancel);
	void currentChanged(int index);

private slots:

	void buttonClicked(int id);

private:
	int _selectedIndex;
	QVBoxLayout *_layout;
	QButtonGroup *_buttonGroup;
};

#endif // VERTICALTABBAR_H
