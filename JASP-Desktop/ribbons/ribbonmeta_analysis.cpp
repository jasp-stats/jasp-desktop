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

#include "ribbonmeta_analysis.h"
#include "ui_ribbonmeta_analysis.h"

#include <QMenu>
#include <QDebug>

RibbonMetaAnalysis::RibbonMetaAnalysis(QWidget *parent) :
	RibbonWidget(parent),
    ui(new Ui::RibbonMetaAnalysis)
{
	ui->setupUi(this);

    addRibbonButton(ui->classicalButton);
    addRibbonButton(ui->multilevelButton);
    addRibbonButton(ui->twoxtwoTablesButton);
    /* a dataset is needed, but leave this here for later reference (e.g., for interfacing metafor::escalc)
     * ui->bfFromTButton->setDataSetNotNeeded();
     * ui->regressionButton->setDataSetNotNeeded();
     * ui->frequenciesButton->setDataSetNotNeeded();
     */

	QMenu *menu;

	menu = new QMenu(this);
    menu->addAction(QString("Classical meta-analysis"), this, SLOT(itemSelected()))->setObjectName("ClassicalMetaAnalysis");

    ui->classicalButton->setMenu(menu);

	menu = new QMenu(this);
    menu->addAction(QString("Multilevel meta-analysis"), this, SLOT(itemSelected()))->setObjectName("MultiLevelMetaAnalysis");
	
    ui->multilevelButton->setMenu(menu);

	menu = new QMenu(this);
    menu->addAction(QString("Meta-analysis of 2 by 2 tables"), this, SLOT(itemSelected()))->setObjectName("TwoxtwoMetaAnalysis");

    ui->twoxtwoTablesButton->setMenu(menu);
}

RibbonMetaAnalysis::~RibbonMetaAnalysis()
{
	delete ui;
}
