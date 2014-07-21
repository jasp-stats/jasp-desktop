
#include "anovarepeatedmeasuresshortform.h"
#include "ui_anovarepeatedmeasuresshortform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

AnovaRepeatedMeasuresShortForm::AnovaRepeatedMeasuresShortForm(QWidget *parent) :
	AnalysisForm("AnovaRepeatedMeasuresShortForm", parent),
	ui(new Ui::AnovaRepeatedMeasuresShortForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_designTableModel = new TableModelAnovaDesign(this);
	ui->design->setModel(_designTableModel);

	_fixedFactorsListModel = new TableModelVariablesAssigned(this);
	_fixedFactorsListModel->setSource(&_availableVariablesModel);
	_fixedFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->fixedFactors->setModel(_fixedFactorsListModel);

	_randomFactorsListModel = new TableModelVariablesAssigned(this);
	_randomFactorsListModel->setSource(&_availableVariablesModel);
	_randomFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->randomFactors->setModel(_randomFactorsListModel);

	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->fixedFactors);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->randomFactors);

	connect(_fixedFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_randomFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	connect(_anovaModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	termsChanged();

	_contrastsModel = new TableModelVariablesOptions();
    ui->contrasts->setModel(_contrastsModel);

	ui->containerModel->hide();
	ui->containerFactors->hide();
	ui->containerOptions->hide();
	ui->containerPostHocTests->hide();

}

AnovaRepeatedMeasuresShortForm::~AnovaRepeatedMeasuresShortForm()
{
	delete ui;
}

void AnovaRepeatedMeasuresShortForm::factorsChanged()
{
	Terms factorsAvailable;

	factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_randomFactorsListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);
	_contrastsModel->setVariables(factorsAvailable);

	ui->postHocTests_variables->setVariables(factorsAvailable);
}

void AnovaRepeatedMeasuresShortForm::termsChanged()
{
	Terms terms = _anovaModel->terms();
	terms.insert(0, string("~OVERALL"));
	ui->marginalMeans_terms->setVariables(terms);
}
