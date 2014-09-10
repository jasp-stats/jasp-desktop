#include "ancovabayesianform.h"
#include "ui_ancovabayesianform.h"

AncovaBayesianForm::AncovaBayesianForm(QWidget *parent) :
	AnalysisForm("AncovaBayesianForm", parent),
	ui(new Ui::AncovaBayesianForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_dependentListModel = new TableModelVariablesAssigned(this);
	_dependentListModel->setVariableTypesSuggested(Column::ColumnTypeScale | Column::ColumnTypeOrdinal);
	_dependentListModel->setSource(&_availableVariablesModel);
	ui->dependent->setModel(_dependentListModel);

	_fixedFactorsListModel = new TableModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableVariablesModel);
	_fixedFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	_randomFactorsListModel = new TableModelVariablesAssigned(this);
	_randomFactorsListModel->setSource(&_availableVariablesModel);
	_randomFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->randomFactors->setModel(_randomFactorsListModel);

	_covariatesListModel = new TableModelVariablesAssigned(this);
	_covariatesListModel->setSource(&_availableVariablesModel);
	_covariatesListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_covariatesListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->covariates->setModel(_covariatesListModel);

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->covariates);

	connect(_dependentListModel, SIGNAL(assignmentsChanged()), this, SLOT(dependentChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	ui->modelTerms->hide();

#ifdef QT_NO_DEBUG
	// temporary hides until the appropriate R code is implemented

	ui->posteriorDistributions->hide();
	ui->posteriorEstimates->hide();
#else
	ui->posteriorDistributions->setStyleSheet("background-color: pink;");
	ui->posteriorEstimates->setStyleSheet("background-color: pink;");
#endif
}

AncovaBayesianForm::~AncovaBayesianForm()
{
	delete ui;
}

/*void AncovaBayesianForm::set(Options *options, DataSet *dataSet)
{
	OptionVariables *nuisanceOption = dynamic_cast<OptionVariables *>(options->get("nuisanceTerms"));

	_anovaModel->setNuisanceTermsOption(nuisanceOption);

	AnalysisForm::set(options, dataSet);
}*/

void AncovaBayesianForm::factorsChanged()
{
	_anovaModel->setVariables(_fixedFactorsListModel->assigned());
}

void AncovaBayesianForm::dependentChanged()
{
	/*const QList<ColumnInfo> &assigned = _dependentListModel->assigned();
	if (assigned.length() == 0)
		_anovaModel->setDependent(ColumnInfo("", 0));
	else
		_anovaModel->setDependent(assigned.last());*/
}
