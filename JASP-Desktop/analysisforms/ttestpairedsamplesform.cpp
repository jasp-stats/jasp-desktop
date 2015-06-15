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
	model->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->pairs->setModel(model);

	ui->assignButton->setSourceAndTarget(ui->availableFields, ui->pairs);

	ui->confidenceIntervalInterval->setLabel("Confidence interval");
	ui->descriptivesPlotsConfidenceInterval->setLabel("Confidence interval");
}

TTestPairedSamplesForm::~TTestPairedSamplesForm()
{
	delete ui;
}
