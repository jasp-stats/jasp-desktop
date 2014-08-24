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

	_layersModel = new TableModelVariablesLevels();
	_layersModel->setSource(&_availableVariablesModel);
	//_layersModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->layers->setModel(_layersModel);

	ui->buttonAssignRows->setSourceAndTarget(ui->listAvailableFields, ui->rows);
	ui->buttonAssignColumns->setSourceAndTarget(ui->listAvailableFields, ui->columns);
	ui->buttonAssignLayers->setSourceAndTarget(ui->listAvailableFields, ui->layers);

	ui->panelStatistics->hide();
	ui->panelCells->hide();
	ui->rowOrder->hide();

#ifdef QT_NO_DEBUG
	ui->groupStatistics->hide();
	ui->groupCellDisplay->hide();
	ui->groupOptions->hide();
#else
	ui->groupStatistics->setStyleSheet("background-color: pink;");
	ui->groupCellDisplay->setStyleSheet("background-color: pink;");
	ui->groupOptions->setStyleSheet("background-color: pink;");
#endif

}

CrosstabsForm::~CrosstabsForm()
{
	delete ui;
}
