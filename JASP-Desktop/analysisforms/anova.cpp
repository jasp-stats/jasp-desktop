#include "anova.h"
#include "ui_anova.h"

Anova::Anova(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::ANOVA)
{
	ui->setupUi(this);
}

Anova::~Anova()
{
	delete ui;
}
