
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
	ui->repeatedMeasuresFactors->setModel(_designTableModel);

	_withinSubjectCellsListModel = new TableModelAnovaWithinSubjectCells(this);
	_withinSubjectCellsListModel->setSource(&_availableVariablesModel);
	_withinSubjectCellsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_withinSubjectCellsListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->repeatedMeasuresCells->setModel(_withinSubjectCellsListModel);

	_randomFactorsListModel = new TableModelVariablesAssigned(this);
	_randomFactorsListModel->setSource(&_availableVariablesModel);
	_randomFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->betweenSubjectFactors->setModel(_randomFactorsListModel);

	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->repeatedMeasuresCells);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->betweenSubjectFactors);

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

	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));

#ifdef QT_NO_DEBUG
	ui->groupModel->hide();
	ui->groupContrasts->hide();
	ui->groupOptions->hide();
	ui->groupPostHoc->hide();
#else
	ui->groupModel->setStyleSheet("background-color: pink ;");
	ui->groupContrasts->setStyleSheet("background-color: pink ;");
	ui->groupOptions->setStyleSheet("background-color: pink ;");
	ui->groupPostHoc->setStyleSheet("background-color: pink ;");
#endif
}

AnovaRepeatedMeasuresShortForm::~AnovaRepeatedMeasuresShortForm()
{
	delete ui;
}

void AnovaRepeatedMeasuresShortForm::factorsChanged()
{
	Terms factorsAvailable;

	//factorsAvailable.add(_fixedFactorsListModel->assigned());
	factorsAvailable.add(_randomFactorsListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);
	_contrastsModel->setVariables(factorsAvailable);

	ui->postHocTests_variables->setVariables(factorsAvailable);
}

void AnovaRepeatedMeasuresShortForm::termsChanged()
{
	Terms terms;

	terms.add(string("~OVERALL"));
	terms.add(_anovaModel->terms());

	ui->marginalMeans_terms->setVariables(terms);
}

void AnovaRepeatedMeasuresShortForm::withinSubjectsDesignChanged()
{
	_withinSubjectCellsListModel->setDesign(_designTableModel->design());
}
