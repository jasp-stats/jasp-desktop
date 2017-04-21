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

#include "ribbonanalysis.h"
#include "ui_ribbonanalysis.h"

#include <QMenu>
#include <QDebug>

RibbonAnalysis::RibbonAnalysis(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonAnalysis)
{
	ui->setupUi(this);

	addRibbonButton(ui->Descriptives);
	addRibbonButton(ui->ttestButton);
	addRibbonButton(ui->anovaButton);
	addRibbonButton(ui->frequenciesButton);
	addRibbonButton(ui->regressionButton);
	addRibbonButton(ui->BFFromT);
	addRibbonButton(ui->factoranalysisButton);

	ui->BFFromT->setDataSetNotNeeded();


//	connect(ui->Descriptives, SIGNAL(clicked()), this, SLOT(itemSelected()));

	QMenu *menu;

	menu = new QMenu(this);
	menu->addAction(QString("Descriptive Statistics"), this, SLOT(itemSelected()))->setObjectName("Descriptives");
	menu->addAction(QString("Reliability Analysis"), this, SLOT(itemSelected()))->setObjectName("ReliabilityAnalysis");

	ui->Descriptives->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("Independent Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestIndependentSamples");
	menu->addAction(QString("Paired Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestPairedSamples");
	menu->addAction(QString("One Sample T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestOneSample");

	menu->addSeparator();

	menu->addAction(QString("Bayesian Independent Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestBayesianIndependentSamples");
	menu->addAction(QString("Bayesian Paired Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestBayesianPairedSamples");
	menu->addAction(QString("Bayesian One Sample T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestBayesianOneSample");

	ui->ttestButton->setMenu(menu);


	menu = new QMenu(this);

	menu->addAction(QString("ANOVA"), this, SLOT(itemSelected()))->setObjectName("Anova");
	menu->addAction(QString("Repeated Measures ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaRepeatedMeasures");
	menu->addAction(QString("ANCOVA"), this, SLOT(itemSelected()))->setObjectName("Ancova");

	menu->addSeparator();

	menu->addAction(QString("Bayesian ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaBayesian");
	menu->addAction(QString("Bayesian Repeated Measures ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaRepeatedMeasuresBayesian");
	menu->addAction(QString("Bayesian ANCOVA"), this, SLOT(itemSelected()))->setObjectName("AncovaBayesian");

	ui->anovaButton->setMenu(menu);


	menu = new QMenu(this);

	menu->addAction(QString("Correlation Matrix"), this, SLOT(itemSelected()))->setObjectName("Correlation");
	menu->addAction(QString("Linear Regression"), this, SLOT(itemSelected()))->setObjectName("RegressionLinear");

	menu->addSeparator();

	menu->addAction(QString("Bayesian Correlation Matrix"), this, SLOT(itemSelected()))->setObjectName("CorrelationBayesian");
	menu->addAction(QString("Bayesian Correlation Pairs"), this, SLOT(itemSelected()))->setObjectName("CorrelationBayesianPairs");
	menu->addAction(QString("Bayesian Linear Regression"), this, SLOT(itemSelected()))->setObjectName("RegressionLinearBayesian");

#ifdef QT_DEBUG
	menu->addSeparator();
	menu->addAction(QString("BAS Regression Linear link"), this, SLOT(itemSelected()))->setObjectName("BASRegressionLinearLink");
#endif
	ui->regressionButton->setMenu(menu);


	menu = new QMenu(this);

	menu->addAction(QString("Binomial Test"), this, SLOT(itemSelected()))->setObjectName("BinomialTest");
	menu->addAction(QString("Contingency Tables"), this, SLOT(itemSelected()))->setObjectName("ContingencyTables");
    menu->addAction(QString("Log-Linear Regression"), this, SLOT(itemSelected()))->setObjectName("RegressionLogLinear");

	menu->addSeparator();
	menu->addAction(QString("Bayesian Binomial Test"), this, SLOT(itemSelected()))->setObjectName("BinomialTestBayesian");
	menu->addAction(QString("Bayesian Contingency Tables"), this, SLOT(itemSelected()))->setObjectName("ContingencyTablesBayesian");
    menu->addAction(QString("Bayesian Log-Linear Regression"), this, SLOT(itemSelected()))->setObjectName("RegressionLogLinearBayesian");

	ui->frequenciesButton->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("Principal Component Analysis"), this, SLOT(itemSelected()))->setObjectName("PrincipalComponentAnalysis");
	menu->addAction(QString("Exploratory Factor Analysis"), this, SLOT(itemSelected()))->setObjectName("ExploratoryFactorAnalysis");

	ui->factoranalysisButton->setMenu(menu);

#ifndef QT_DEBUG
	ui->BFFromT->hide();
#else
	menu = new QMenu(this);
	menu->addAction(QString("BF From t"), this, SLOT(itemSelected()))->setObjectName("BFFromT");

	ui->BFFromT->setMenu(menu);
#endif
}

RibbonAnalysis::~RibbonAnalysis()
{
	delete ui;
}
