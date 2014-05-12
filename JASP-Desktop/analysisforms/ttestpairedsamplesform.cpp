#include "ttestpairedsamplesform.h"
#include "ui_ttestpairedsamplesform.h"

#include "widgets/tablemodelvariablesassigned.h"

TTestPairedSamplesForm::TTestPairedSamplesForm(QWidget *parent) :
	AnalysisForm("TTestPairedSamplesForm", parent),
	ui(new Ui::TTestPairedSamplesForm)
{
	ui->setupUi(this);

	_availableFields.setSupportedDropActions(Qt::MoveAction);
	_availableFields.setSupportedDragActions(Qt::CopyAction);
	_availableFields.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableFields.setIsNominalTextAllowed(false);
	ui->availableFields->setModel(&_availableFields);
	ui->availableFields->setDefaultDropAction(Qt::MoveAction);
	ui->availableFields->setDoubleClickTarget(ui->pairs);

	TableModelVariablesAssigned *model = new TableModelVariablesAssigned(this);
	model->setSource(&_availableFields);
	model->setVariableTypesSuggested(Column::ColumnTypeScale);
	model->setIsNominalTextAllowed(false);
	ui->pairs->setModel(model);

	ui->assignButton->setSourceAndTarget(ui->availableFields, ui->pairs);
}

TTestPairedSamplesForm::~TTestPairedSamplesForm()
{
	delete ui;
}
