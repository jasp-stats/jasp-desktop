#include "regressionlinearform.h"
#include "ui_regressionlinearform.h"

RegressionLinearForm::RegressionLinearForm(QWidget *parent) :
	AnalysisForm("RegressionLinearForm", parent),
	ui(new Ui::RegressionLinearForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&this->_availableFields);

	_dependentModel = new ListModelVariablesAssigned();
	_dependentModel->setSource(&_availableFields);
	_dependentModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->dependent->setModel(_dependentModel);

	_blocksModel = new TableModelVariablesLevels();
	_blocksModel->setSource(&_availableFields);
	//_layersModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->blocks->setModel(_blocksModel);

	_wlsWeightsModel = new ListModelVariablesAssigned();
	_wlsWeightsModel->setSource(&_availableFields);
	_wlsWeightsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->wlsWeights->setModel(_wlsWeightsModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignBlocks->setSourceAndTarget(ui->listAvailableFields, ui->blocks);
	ui->buttonAssignWlsWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	ui->panelStatistics->hide();
	ui->panelOptions->hide();
}

RegressionLinearForm::~RegressionLinearForm()
{
	delete ui;
}
