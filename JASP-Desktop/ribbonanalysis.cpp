#include "ribbonanalysis.h"
#include "ui_ribbonanalysis.h"

#include <QMenu>
#include <QDebug>

RibbonAnalysis::RibbonAnalysis(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::RibbonAnalysis)
{
	ui->setupUi(this);

	QMenu *menu = new QMenu(this);
	menu->addAction(QString("Independent Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestIndependentSamples");
	menu->addAction(QString("Paired Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestPairedSamples");
	menu->addAction(QString("One Sample T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestOneSample");

	ui->TTest->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("One Way"), this, SLOT(itemSelected()))->setObjectName("AnovaOneWay");
	menu->addAction(QString("Univariate"), this, SLOT(itemSelected()))->setObjectName("AnovaUnivariate");
	menu->addAction(QString("Bayesian"), this, SLOT(itemSelected()))->setObjectName("AnovaBayesian");
	menu->addAction(QString("Multivariate"), this, SLOT(itemSelected()))->setObjectName("AnovaMultivariate");

	ui->Anova->setMenu(menu);
}

RibbonAnalysis::~RibbonAnalysis()
{
	delete ui;
}

void RibbonAnalysis::itemSelected()
{
	QObject *source = this->sender();
	QString name = source->objectName();

	emit itemSelected(name);
}

void RibbonAnalysis::menuItemSelected()
{
	QPushButton *source = dynamic_cast<QPushButton*>(this->sender());
	if (source != NULL)
		source->showMenu();
}
