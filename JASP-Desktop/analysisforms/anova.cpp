#include "anova.h"
#include "ui_anova.h"

ANOVA::ANOVA(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::ANOVA)
{
	ui->setupUi(this);
}

ANOVA::~ANOVA()
{
	delete ui;
}
