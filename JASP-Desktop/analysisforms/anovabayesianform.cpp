#include "anovabayesianform.h"
#include "ui_anovabayesianform.h"

AnovaBayesianForm::AnovaBayesianForm(QWidget *parent) :
	AnalysisForm("AnovaBayesianForm", parent),
	ui(new Ui::AnovaBayesianForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setSource(&_availableVariablesModel);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_dependentListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	ui->dependent->setModel(_dependentListModel);

	_fixedFactorsListModel = new TableModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableVariablesModel);
	_fixedFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	_randomFactorsListModel = new TableModelVariablesAssigned(this);
	_randomFactorsListModel->setSource(&_availableVariablesModel);
	_randomFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->randomFactors->setModel(_randomFactorsListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	ui->modelTerms->hide();

	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(assignmentsChanging()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(assignmentsChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addFixedFactors(Terms)));
	connect(_fixedFactorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

	connect(_randomFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(assignmentsChanging()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(assignmentsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignedTo(Terms)), _anovaModel, SLOT(addRandomFactors(Terms)));
	connect(_randomFactorsListModel, SIGNAL(unassigned(Terms)), _anovaModel, SLOT(removeVariables(Terms)));

#ifdef QT_NO_DEBUG
	// temporary hides until the appropriate R code is implemented

	ui->posteriorDistributions->hide();
	ui->posteriorEstimates->hide();

#else
	ui->posteriorDistributions->setStyleSheet("background-color: pink;");
	ui->posteriorEstimates->setStyleSheet("background-color: pink;");
#endif
}

AnovaBayesianForm::~AnovaBayesianForm()
{
	delete ui;
}

void AnovaBayesianForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	factorsChanging();

	_anovaModel->setVariables(_fixedFactorsListModel->assigned(), _randomFactorsListModel->assigned());

	factorsChanged();
}

void AnovaBayesianForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void AnovaBayesianForm::factorsChanged()
{
	if (_options != NULL)
		_options->blockSignals(false);
}



