#include "crosstabsform.h"
#include "ui_crosstabsform.h"

CrosstabsForm::CrosstabsForm(QWidget *parent) :
	AnalysisForm("Crosstabs", parent),
	ui(new Ui::CrosstabsForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableFields);

	_rowsModel = new ListModelVariablesAssigned();
	_rowsModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_rowsModel->setSource(&_availableFields);
	ui->rows->setModel(_rowsModel);

	_columnsModel = new ListModelVariablesAssigned();
	_columnsModel->setSource(&_availableFields);
	_columnsModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->columns->setModel(_columnsModel);

	_layersModel = new TableModelVariablesLevels();
	_layersModel->setSource(&_availableFields);
	//_layersModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->layers->setModel(_layersModel);

	ui->buttonAssignRows->setSourceAndTarget(ui->listAvailableFields, ui->rows);
	ui->buttonAssignColumns->setSourceAndTarget(ui->listAvailableFields, ui->columns);
	ui->buttonAssignLayers->setSourceAndTarget(ui->listAvailableFields, ui->layers);

	ui->groupStatistics->hide();
	ui->groupCellDisplay->hide();
	ui->groupTableFormat->hide();
}

CrosstabsForm::~CrosstabsForm()
{
	delete ui;
}
