#include "contingencytablesform.h"
#include "ui_contingencytablesform.h"

ContingencyTablesForm::ContingencyTablesForm(QWidget *parent) :
	AnalysisForm("ContingencyTablesForm", parent),
	ui(new Ui::ContingencyTablesForm)
{
	ui->setupUi(this);

	_availableFields.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->listAvailableFields->setModel(&_availableFields);

	_rowsListModel = new ListModelVariablesAssigned(this);
	_rowsListModel->setSource(&_availableFields);
	_rowsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->rows->setModel(_rowsListModel);

	_columnsListModel = new ListModelVariablesAssigned(this);
	_columnsListModel->setSource(&_availableFields);
	_columnsListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->columns->setModel(_columnsListModel);

}

ContingencyTablesForm::~ContingencyTablesForm()
{
	delete ui;
}
