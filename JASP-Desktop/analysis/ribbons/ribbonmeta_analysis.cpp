//
// Copyright (C) 2018 University of Amsterdam
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
	ui->classicalButton->setObjectName("ClassicalMetaAnalysis");
	
#ifdef JASP_DEBUG
    addRibbonButton(ui->multilevelButton);
    addRibbonButton(ui->twoxtwoTablesButton);
	ui->multilevelButton->setObjectName("MultiLevelMetaAnalysis");
	ui->twoxtwoTablesButton->setObjectName("TwoxtwoMetaAnalysis");
#else
    ;
#endif
    /* a dataset is needed, but leave this here for later reference (e.g., for interfacing metafor::escalc)
     * ui->bfFromTButton->setDataSetNotNeeded();
     * ui->regressionButton->setDataSetNotNeeded();
     * ui->frequenciesButton->setDataSetNotNeeded();
     */

	connect(ui->classicalButton, SIGNAL(clicked()), this, SLOT(itemSelected()));
	
#ifdef JASP_DEBUG
	connect(ui->twoxtwoTablesButton, SIGNAL(clicked()), this, SLOT(itemSelected()));
	connect(ui->multilevelButton, SIGNAL(clicked()), this, SLOT(itemSelected()));	
#else
	ui->twoxtwoTablesButton->hide();
	ui->multilevelButton->hide();
#endif
}

RibbonMetaAnalysis::~RibbonMetaAnalysis()
{
	delete ui;
}
