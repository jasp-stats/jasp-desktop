#include "anova.h"
#include "ui_anova.h"

#include "column.h"
#include "widgets/listmodelvariablesassigned.h"

Anova::Anova(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::ANOVA)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);

	ListModelVariablesAssigned *dependentListModel = new ListModelVariablesAssigned(this);
	dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	dependentListModel->setSource(&_availableFields);
	ui->dependent->setModel(dependentListModel);

	ListModelVariablesAssigned *fixedFactorsListModel = new ListModelVariablesAssigned(this);
	fixedFactorsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(fixedFactorsListModel);

	ListModelVariablesAssigned *randomFactorsListModel = new ListModelVariablesAssigned(this);
	randomFactorsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->randomFactors->setModel(randomFactorsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);

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
