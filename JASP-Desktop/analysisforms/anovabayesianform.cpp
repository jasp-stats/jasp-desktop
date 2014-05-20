#include "anovabayesianform.h"
#include "ui_anovabayesianform.h"

AnovaBayesianForm::AnovaBayesianForm(QWidget *parent) :
	AnalysisForm("AnovaBayesianForm", parent),
	ui(new Ui::AnovaBayesianForm)
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

	ui->buttonAssignDependent->setSourceAndTarget(ui->listAvailableFields, ui->dependent);
	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);

	connect(_dependentListModel, SIGNAL(assignmentsChanged()), this, SLOT(dependentChanged()));
	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	ui->modelTerms->hide();
}

AnovaBayesianForm::~AnovaBayesianForm()
{
	delete ui;
}

/*void AnovaBayesianForm::set(Options *options, DataSet *dataSet)
{
	OptionVariables *nuisanceOption = dynamic_cast<OptionVariables *>(options->get("nuisanceTerms"));

	_anovaModel->setNuisanceTermsOption(nuisanceOption);

	AnalysisForm::set(options, dataSet);
}*/

void AnovaBayesianForm::factorsChanged()
{
	/*QList<ColumnInfo> factorsAvailable;

	factorsAvailable.append(_fixedFactorsListModel->assigned());
	factorsAvailable.append(_randomFactorsListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);*/
}

void AnovaBayesianForm::dependentChanged()
{
	/*const QList<ColumnInfo> &assigned = _dependentListModel->assigned();
	if (assigned.length() == 0)
		_anovaModel->setDependent(ColumnInfo("", 0));
	else
		_anovaModel->setDependent(assigned.last());*/
}
