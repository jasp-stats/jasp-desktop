
#include "anovarepeatedmeasuresbayesianform.h"
#include "ui_anovarepeatedmeasuresbayesianform.h"

#include "column.h"
#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodelnuisancefactors.h"

AnovaRepeatedMeasuresBayesianForm::AnovaRepeatedMeasuresBayesianForm(QWidget *parent) :
	AnalysisForm("AnovaRepeatedMeasuresBayesianForm", parent),
	ui(new Ui::AnovaRepeatedMeasuresBayesianForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);

	_designTableModel = new TableModelAnovaDesign(this);
	ui->repeatedMeasuresFactors->setModel(_designTableModel);

	// this is a hack to allow deleting factors and levels :/
	// ideally this would be handled between the TableView and the model
	// and wouldn't require the surrounding classes' intervention like this
	connect(ui->repeatedMeasuresFactors, SIGNAL(clicked(QModelIndex)), this, SLOT(anovaDesignTableClicked(QModelIndex)));

	_withinSubjectCellsListModel = new TableModelAnovaWithinSubjectCells(this);
	_withinSubjectCellsListModel->setSource(&_availableVariablesModel);
	_withinSubjectCellsListModel->setVariableTypesSuggested(Column::ColumnTypeScale);
	_withinSubjectCellsListModel->setVariableTypesAllowed(Column::ColumnTypeScale | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->repeatedMeasuresCells->setModel(_withinSubjectCellsListModel);

	_betweenSubjectsFactorsListModel = new TableModelVariablesAssigned(this);
	_betweenSubjectsFactorsListModel->setSource(&_availableVariablesModel);
	_betweenSubjectsFactorsListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	ui->betweenSubjectFactors->setModel(_betweenSubjectsFactorsListModel);

	ui->buttonAssignFixed->setSourceAndTarget(ui->listAvailableFields, ui->repeatedMeasuresCells);
	ui->buttonAssignRandom->setSourceAndTarget(ui->listAvailableFields, ui->betweenSubjectFactors);

	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));

	_anovaModel = new TableModelAnovaModel(this);
	ui->modelTerms->setModel(_anovaModel);
	connect(_anovaModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	termsChanged();

	ui->containerModel->hide();

	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));

#ifdef QT_NO_DEBUG
	ui->modelContainer->hide();
#else
	ui->modelContainer->setStyleSheet("background-color: pink ;");
#endif
}

AnovaRepeatedMeasuresBayesianForm::~AnovaRepeatedMeasuresBayesianForm()
{
	delete ui;
}

void AnovaRepeatedMeasuresBayesianForm::factorsChanged()
{
	Terms factorsAvailable;

	foreach (const Factor &factor, _designTableModel->design())
		factorsAvailable.add(factor.first);

	factorsAvailable.add(_betweenSubjectsFactorsListModel->assigned());

	_anovaModel->setVariables(factorsAvailable);
}

void AnovaRepeatedMeasuresBayesianForm::termsChanged()
{

}

void AnovaRepeatedMeasuresBayesianForm::withinSubjectsDesignChanged()
{
	_withinSubjectCellsListModel->setDesign(_designTableModel->design());
	factorsChanged();
}

void AnovaRepeatedMeasuresBayesianForm::anovaDesignTableClicked(QModelIndex index)
{
	// the second column contains an X to delete the row

	if (index.column() == 1)
		_designTableModel->removeRow(index.row());
}
