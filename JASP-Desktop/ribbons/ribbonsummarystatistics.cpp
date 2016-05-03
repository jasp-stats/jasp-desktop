//
// Copyright (C) 2016 University of Amsterdam
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

#include "ribbonsummarystatistics.h"
#include "ui_ribbonsummarystatistics.h"

#include <QMenu>
#include <QDebug>

RibbonSummaryStatistics::RibbonSummaryStatistics(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonSummaryStatistics)
{
	ui->setupUi(this);

	addRibbonButton(ui->BFFromT);

	ui->BFFromT->setDataSetNotNeeded();

	QMenu *menu;

	menu = new QMenu(this);
	menu->addAction(QString("Independent Samples"), this, SLOT(itemSelected()))->setObjectName("BFFromT");
	menu->addAction(QString("Paired Samples"), this, SLOT(itemSelected()))->setObjectName("BFFromT");
	menu->addAction(QString("One Sample"), this, SLOT(itemSelected()))->setObjectName("BFFromT");

	ui->BFFromT->setMenu(menu);
}

RibbonSummaryStatistics::~RibbonSummaryStatistics()
{
	delete ui;
}
