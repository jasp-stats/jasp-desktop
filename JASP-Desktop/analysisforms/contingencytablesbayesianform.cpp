#include "contingencytablesbayesianform.h"
#include "ui_contingencytablesbayesianform.h"

ContingencyTablesBayesianForm::ContingencyTablesBayesianForm(QWidget *parent) :
	AnalysisForm("ContingencyTablesBayesianForm", parent),
	ui(new Ui::ContingencyTablesBayesianForm)
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
	ui->panelOptions->hide();

	ui->oddsRatioCredibleIntervalInterval->setLabel("Credible interval");
}

ContingencyTablesBayesianForm::~ContingencyTablesBayesianForm()
{
	delete ui;
}

void ContingencyTablesBayesianForm::otherSamplingToggled(bool on)
{
	if (on)
		ui->hypothesis->setEnabled(false);
}

void ContingencyTablesBayesianForm::independentMultinomialSamplingToggled(bool on)
{
	if (on)
		ui->hypothesis->setEnabled(true);
}
