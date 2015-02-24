#include "ttestbayesianpairedsamplesform.h"
#include "ui_ttestbayesianpairedsamplesform.h"

#include "widgets/tablemodelvariablesassigned.h"

TTestBayesianPairedSamplesForm::TTestBayesianPairedSamplesForm(QWidget *parent) :
	AnalysisForm("TTestBayesianPairedSamplesForm", parent),
	ui(new Ui::TTestBayesianPairedSamplesForm)
{
	ui->setupUi(this);

	_availableVariablesModel.setSupportedDropActions(Qt::MoveAction);
	_availableVariablesModel.setSupportedDragActions(Qt::CopyAction);
	_availableVariablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	_availableVariablesModel.setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);

	ui->availableFields->setModel(&_availableVariablesModel);
	ui->availableFields->setDefaultDropAction(Qt::MoveAction);
	ui->availableFields->setDoubleClickTarget(ui->pairs);

	TableModelPairsAssigned *model = new TableModelPairsAssigned(this);
	model->setSource(&_availableVariablesModel);
	model->setVariableTypesSuggested(Column::ColumnTypeScale);
	model->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->pairs->setModel(model);

	ui->assignButton->setSourceAndTarget(ui->availableFields, ui->pairs);

#ifdef QT_NO_DEBUG
	ui->plotBayesFactorRobustness->hide();
	ui->plotSequentialAnalysis->hide();
	ui->plotSequentialAnalysisRobustness->hide();
#else
	ui->plotBayesFactorRobustness->setStyleSheet("background-color: pink;");
	ui->plotSequentialAnalysis->setStyleSheet("background-color: pink;");
	ui->plotSequentialAnalysisRobustness->setStyleSheet("background-color: pink;");
#endif
}

TTestBayesianPairedSamplesForm::~TTestBayesianPairedSamplesForm()
{
	delete ui;
}
