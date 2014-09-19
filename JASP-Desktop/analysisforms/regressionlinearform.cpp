#include "regressionlinearform.h"
#include "ui_regressionlinearform.h"

RegressionLinearForm::RegressionLinearForm(QWidget *parent) :
	AnalysisForm("RegressionLinearForm", parent),
	ui(new Ui::RegressionLinearForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&this->_availableVariablesModel);

	_dependentModel = new TableModelVariablesAssigned();
	_dependentModel->setSource(&_availableVariablesModel);
	_dependentModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->dependent->setModel(_dependentModel);

	_blocksModel = new TableModelVariablesLevels();
	_blocksModel->setSource(&_availableVariablesModel);
	//_layersModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->blocks->setModel(_blocksModel);

	_wlsWeightsModel = new TableModelVariablesAssigned();
	_wlsWeightsModel->setSource(&_availableVariablesModel);
	_wlsWeightsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->wlsWeights->setModel(_wlsWeightsModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignBlocks->setSourceAndTarget(ui->listAvailableFields, ui->blocks);
	ui->buttonAssignWlsWeights->setSourceAndTarget(ui->listAvailableFields, ui->wlsWeights);

	ui->panelStatistics->hide();
	ui->panelOptions->hide();

#ifdef QT_NO_DEBUG
	ui->groupBoxResiduals->hide();
	ui->groupBoxSteppingMethodCriteria->hide();
	ui->partAndPartialCorrelations->hide();
	ui->collinearityDiagnostics->hide();
	ui->includeConstant->hide();
#else
	ui->groupBoxResiduals->setStyleSheet("background-color: pink ;");
	ui->groupBoxSteppingMethodCriteria->setStyleSheet("background-color: pink ;");
	ui->partAndPartialCorrelations->setStyleSheet("background-color: pink ;");
	ui->collinearityDiagnostics->setStyleSheet("background-color: pink ;");
	ui->includeConstant->setStyleSheet("background-color: pink ;");
#endif
}

RegressionLinearForm::~RegressionLinearForm()
{
	delete ui;
}
