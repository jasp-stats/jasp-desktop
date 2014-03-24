#include "ttestbayesianpairedsamplesform.h"
#include "ui_ttestbayesianpairedsamplesform.h"

#include "widgets/tablemodelvariablesassigned.h"

TTestBayesianPairedSamplesForm::TTestBayesianPairedSamplesForm(QWidget *parent) :
	AnalysisForm("TTestBayesianPairedSamplesForm", parent),
	ui(new Ui::TTestBayesianPairedSamplesForm)
{
	ui->setupUi(this);

	_availableFields.setSupportedDropActions(Qt::MoveAction);
	_availableFields.setSupportedDragActions(Qt::CopyAction);
	_availableFields.setVariableTypesAllowed(Column::ColumnTypeScale);
	ui->availableFields->setModel(&_availableFields);
	ui->availableFields->setDefaultDropAction(Qt::MoveAction);
	ui->availableFields->setDoubleClickTarget(ui->pairs);

	TableModelVariablesAssigned *model = new TableModelVariablesAssigned(this);
	model->setSource(&_availableFields);
	model->setVariableTypesAllowed(Column::ColumnTypeScale);
	ui->pairs->setModel(model);

	ui->assignButton->setSourceAndTarget(ui->availableFields, ui->pairs);
}

TTestBayesianPairedSamplesForm::~TTestBayesianPairedSamplesForm()
{
	delete ui;
}
