#include "crosstabsbayesianform.h"
#include "ui_crosstabsbayesianform.h"

CrosstabsBayesianForm::CrosstabsBayesianForm(QWidget *parent) :
	AnalysisForm("CrosstabsBayesianForm", parent),
	ui(new Ui::CrosstabsBayesianForm)
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
	ui->rowOrder->hide();

#ifdef QT_NO_DEBUG
	ui->groupStatistics->hide();
	ui->groupCellDisplay->hide();

	ui->groupOddsRatio->hide();
	ui->groupPlotPosterior->hide();
#else
	ui->groupOddsRatio->setStyleSheet("background-color: pink;");
	ui->groupPlotPosterior->setStyleSheet("background-color: pink;");

	ui->groupCellDisplay->setStyleSheet("background-color: pink;");
	ui->groupOptions->setStyleSheet("background-color: pink;");
#endif

}

CrosstabsBayesianForm::~CrosstabsBayesianForm()
{
	delete ui;
}
