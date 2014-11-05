#include "ribbonanalysis.h"
#include "ui_ribbonanalysis.h"

#include <QMenu>
#include <QDebug>

RibbonAnalysis::RibbonAnalysis(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonAnalysis)
{
	ui->setupUi(this);

	connect(ui->Descriptives, SIGNAL(clicked()), this, SLOT(itemSelected()));

	QMenu *menu = new QMenu(this);

	menu->addAction(QString("Independent Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestIndependentSamples");
	menu->addAction(QString("Paired Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestPairedSamples");
	menu->addAction(QString("One Sample T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestOneSample");

	ui->classicalTTestButton->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("One Way ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaOneWay");
	menu->addAction(QString("ANOVA"), this, SLOT(itemSelected()))->setObjectName("Anova");
	menu->addAction(QString("Repeated Measures ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaRepeatedMeasuresShort");
	menu->addAction(QString("Repeated Measures ANOVA [Long Form]"), this, SLOT(itemSelected()))->setObjectName("AnovaRepeatedMeasures");
	menu->addAction(QString("ANCOVA"), this, SLOT(itemSelected()))->setObjectName("Ancova");
	menu->addAction(QString("MAN(C)OVA"), this, SLOT(itemSelected()))->setObjectName("AncovaMultivariate");
	//menu->addAction(QString("MANCOVA"), this, SLOT(itemSelected()))->setObjectName("AncovaMultivariate");

	ui->classicalAnovaButton->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("Correlation"), this, SLOT(itemSelected()))->setObjectName("Correlation");
	menu->addAction(QString("Partial Correlation"), this, SLOT(itemSelected()))->setObjectName("CorrelationPartial");
	menu->addAction(QString("Linear Regression"), this, SLOT(itemSelected()))->setObjectName("RegressionLinear");

	ui->classicalRegressionButton->setMenu(menu);

	connect(ui->Crosstabs, SIGNAL(clicked()), this, SLOT(itemSelected()));

	menu = new QMenu(this);
	menu->addAction(QString("Bayesian Independent Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestBayesianIndependentSamples");
	menu->addAction(QString("Bayesian Paired Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestBayesianPairedSamples");
	menu->addAction(QString("Bayesian One Sample T-Test"), this, SLOT(itemSelected()))->setObjectName("TTestBayesianOneSample");

	ui->bayesianTTestButton->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("Bayesian ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaBayesian");
	menu->addAction(QString("Bayesian Repeated Measures ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaRepeatedMeasuresBayesian");
	menu->addAction(QString("Bayesian ANCOVA"), this, SLOT(itemSelected()))->setObjectName("AncovaBayesian");

	ui->bayesianAnovaButton->setMenu(menu);

	ui->bayesianRegressionButton->hide();

}

RibbonAnalysis::~RibbonAnalysis()
{
	delete ui;
}
