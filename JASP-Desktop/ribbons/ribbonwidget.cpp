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

#include "ribbonwidget.h"

#include <QDebug>
#include <QMenu>
#include "widgets/ribbonbutton.h"

RibbonWidget::RibbonWidget(QWidget *parent) :
	QWidget(parent)
{
}

void RibbonWidget::addRibbonButton(RibbonButton *button)
{
	_buttons.append(button);
}

void RibbonWidget::itemSelected()
{
	QObject *source = this->sender();
	QString name = source->objectName();

	emit itemSelected(name);
}

void RibbonWidget::setDataSetLoaded(bool loaded)
{
	foreach (RibbonButton *button, _buttons)
	{
		if (loaded)
			button->setEnabled(true);
		else if (button->isDataSetNeeded())
			button->setEnabled(false);
	}
}


