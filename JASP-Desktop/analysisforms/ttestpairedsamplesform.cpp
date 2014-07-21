#include "ttestpairedsamplesform.h"
#include "ui_ttestpairedsamplesform.h"

#include "widgets/tablemodelvariablesassigned.h"

TTestPairedSamplesForm::TTestPairedSamplesForm(QWidget *parent) :
	AnalysisForm("TTestPairedSamplesForm", parent),
	ui(new Ui::TTestPairedSamplesForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setSupportedDropActions(Qt::MoveAction);
	_availableVariablesModel.setSupportedDragActions(Qt::CopyAction);
	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->availableFields->setModel(&_availableVariablesModel);
	ui->availableFields->setDefaultDropAction(Qt::MoveAction);
	ui->availableFields->setDoubleClickTarget(ui->pairs);

	TableModelPairsAssigned *model = new TableModelPairsAssigned(this);
	model->setSource(&_availableVariablesModel);
	model->setVariableTypesSuggested(Column::ColumnTypeScale);
	model->setIsNominalTextAllowed(false);
	ui->pairs->setModel(model);

	ui->assignButton->setSourceAndTarget(ui->availableFields, ui->pairs);
}

TTestPairedSamplesForm::~TTestPairedSamplesForm()
{
	delete ui;
}
