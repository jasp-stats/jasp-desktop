#include "anova.h"
#include "ui_anova.h"

Anova::Anova(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::ANOVA)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);

	link(ui->listAvailableFields, ui->buttonAssignDependent, ui->dependent);
	link(ui->listAvailableFields, ui->buttonAssignFixed, ui->fixedFactors);
	link(ui->listAvailableFields, ui->buttonAssignRandom, ui->randomFactors);

}

Anova::~Anova()
{
	delete ui;
}

void Anova::set(Options *options, DataSet *dataSet)
{
	AnalysisForm::set(options, dataSet);

	ui->anovaModel->bindTo(options->get("fixedFactors"), AnovaModelWidget::FIXED_FACTORS);
	ui->anovaModel->bindTo(options->get("randomFactors"), AnovaModelWidget::RANDOM_FACTORS);
	ui->anovaModel->bindTo(options->get("mainEffects"), AnovaModelWidget::MAIN_EFFECTS);

	ui->anovaModel->setDataSet(dataSet);
}
