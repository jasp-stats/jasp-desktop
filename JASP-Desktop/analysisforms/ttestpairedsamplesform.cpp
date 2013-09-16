#include "ttestpairedsamplesform.h"
#include "ui_ttestpairedsamplesform.h"

#include "widgets/tablemodelvariablesassigned.h"

TTestPairedSamplesForm::TTestPairedSamplesForm(QWidget *parent) :
	AnalysisForm(parent),
	ui(new Ui::TTestPairedSamplesForm)
{
	ui->setupUi(this);

	_availableFields.setSupportedDropActions(Qt::MoveAction);
	_availableFields.setSupportedDragActions(Qt::CopyAction);
	_availableFields.setVariableTypesAllowed(Column::ColumnTypeScale);
	ui->availableFields->setModel(&_availableFields);
	ui->availableFields->setDefaultDropAction(Qt::MoveAction);
	ui->availableFields->setDoubleClickTarget(ui->pairs);

	TableModelVariablesAssigned *model = new TableModelVariablesAssigned(this);
	model->setVariableTypesAllowed(Column::ColumnTypeScale);
	ui->pairs->setModel(model);

	ui->assignButton->setSourceAndTarget(ui->availableFields, ui->pairs);
}

TTestPairedSamplesForm::~TTestPairedSamplesForm()
{
	delete ui;
}
