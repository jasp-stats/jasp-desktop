#include "r11tlearnform.h"
#include "ui_r11tlearnform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

R11tLearnForm::R11tLearnForm(QWidget *parent) :
	AnalysisForm("R11tLearnForm", parent),
	ui(new Ui::R11tLearnForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_subjectIdListModel = new TableModelVariablesAssigned(this);
	_subjectIdListModel->setVariableTypesSuggested(Column::ColumnTypeNominal);
	_subjectIdListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	_subjectIdListModel->setSource(&_availableVariablesModel);
	ui->subjectId->setModel(_subjectIdListModel);

	_trialNumberListModel = new TableModelVariablesAssigned(this);
	_trialNumberListModel->setSource(&_availableVariablesModel);
	_trialNumberListModel->setVariableTypesSuggested(Column::ColumnTypeOrdinal);
	ui->trialNumber->setModel(_trialNumberListModel);

	_rewardsListModel = new TableModelVariablesAssigned(this);
	_rewardsListModel->setSource(&_availableVariablesModel);
	_rewardsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_rewardsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->rewards->setModel(_rewardsListModel);

	_lossesListModel = new TableModelVariablesAssigned(this);
	_lossesListModel->setSource(&_availableVariablesModel);
	_lossesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_lossesListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->losses->setModel(_lossesListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->subjectId);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->trialNumber);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->rewards);
	ui->buttonAssignWLSWeights->setSourceAndTarget(ui->listAvailableFields, ui->losses);
}

R11tLearnForm::~R11tLearnForm()
{
	delete ui;
}
