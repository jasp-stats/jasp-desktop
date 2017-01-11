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

#ifndef BOUNDGROUPBOX_H
#define BOUNDGROUPBOX_H

#include <QEvent>
#include <QWidget>
#include <QTimer>
#include <QButtonGroup>
#include <QRadioButton>
#include <QGroupBox>

#include "bound.h"
#include "itemmodelselectitem.h"
#include "groupbox.h"

class BoundGroupBox : public GroupBox, public Bound
{
	Q_OBJECT
public:
	explicit BoundGroupBox(QWidget *parent = 0);

	void bindTo(Option *option) OVERRIDE;


protected:
	void childEvent(QChildEvent *child) OVERRIDE;
	
private:
	QButtonGroup *_buttonGroup;
	QTimer *_timer;
	ItemModelSelectItem _model;

signals:
	
private slots:
	void updateGroup();
	void itemSelected(QAbstractButton *);
	
};

#endif // BOUNDGROUPBOX_H
