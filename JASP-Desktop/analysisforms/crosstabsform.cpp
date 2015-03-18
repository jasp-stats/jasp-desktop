#include "crosstabsform.h"
#include "ui_crosstabsform.h"

CrosstabsForm::CrosstabsForm(QWidget *parent) :
	AnalysisForm("Crosstabs", parent),
	ui(new Ui::CrosstabsForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_rowsModel = new TableModelVariablesAssigned();
	_rowsModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_rowsModel->setSource(&_availableVariablesModel);
	ui->rows->setModel(_rowsModel);

	_columnsModel = new TableModelVariablesAssigned();
	_columnsModel->setSource(&_availableVariablesModel);
	_columnsModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->columns->setModel(_columnsModel);

	_countsModel = new TableModelVariablesAssigned();
	_countsModel->setSource(&_availableVariablesModel);
	_countsModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_countsModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->counts->setModel(_countsModel);

	_layersModel = new TableModelVariablesLevels();
	_layersModel->setSource(&_availableVariablesModel);
	_layersModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->layers->setModel(_layersModel);

	ui->buttonAssignRows->setSourceAndTarget(ui->listAvailableFields, ui->rows);
	ui->buttonAssignColumns->setSourceAndTarget(ui->listAvailableFields, ui->columns);
	ui->buttonAssignCounts->setSourceAndTarget(ui->listAvailableFields, ui->counts);
	ui->buttonAssignLayers->setSourceAndTarget(ui->listAvailableFields, ui->layers);

	ui->panelStatistics->hide();
	ui->panelCells->hide();
	ui->panelOptions->hide();

#ifdef QT_NO_DEBUG
	ui->nominalLambda->hide();
	ui->nominalUncertaintyCoefficient->hide();

	ui->ordinalSomersD->hide();
	ui->ordinalKendallsTauC->hide();

	ui->groupNominalByInterval->hide();
	ui->groupCochrans->hide();

	ui->hideSmallCounts->hide();
	ui->hideSmallCountsLessThan->hide();
	ui->counts_hideSmallCountsLessThanLabel->hide();

	ui->groupZTest->hide();
	ui->groupResiduals->hide();
#else
	ui->nominalLambda->setStyleSheet("background-color: pink;");
	ui->nominalUncertaintyCoefficient->setStyleSheet("background-color: pink;");

	ui->ordinalSomersD->setStyleSheet("background-color: pink;");
	ui->ordinalKendallsTauC->setStyleSheet("background-color: pink;");

	ui->groupNominalByInterval->setStyleSheet("background-color: pink;");
	ui->groupCochrans->setStyleSheet("background-color: pink;");

	ui->hideSmallCounts->setStyleSheet("background-color: pink;");
	ui->hideSmallCountsLessThan->setStyleSheet("background-color: pink;");
	ui->counts_hideSmallCountsLessThanLabel->setStyleSheet("background-color: pink;");

	ui->groupZTest->setStyleSheet("background-color: pink;");
	ui->groupResiduals->setStyleSheet("background-color: pink;");
#endif

}

CrosstabsForm::~CrosstabsForm()
{
	delete ui;
}
