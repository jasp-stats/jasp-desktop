
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

	_withinSubjectsTermsModel = new TableModelAnovaModel(this);
	ui->withinModelTerms->setModel(_withinSubjectsTermsModel);
	connect(_withinSubjectsTermsModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	_betweenSubjectsTermsModel = new TableModelAnovaModel(this);
	ui->betweenModelTerms->setModel(_betweenSubjectsTermsModel);
	connect(_betweenSubjectsTermsModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	_contrastsModel = new TableModelVariablesOptions();
    ui->contrasts->setModel(_contrastsModel);

	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanging()), this, SLOT(factorsChanging()));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignmentsChanged()), this, SLOT(factorsChanged()));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(assignedTo(Terms)), _betweenSubjectsTermsModel, SLOT(addFixedFactors(Terms)));
	connect(_betweenSubjectsFactorsListModel, SIGNAL(unassigned(Terms)), _betweenSubjectsTermsModel, SLOT(removeVariables(Terms)));

	connect(_designTableModel, SIGNAL(designChanging()), this, SLOT(factorsChanging()));
	connect(_designTableModel, SIGNAL(designChanged()), this, SLOT(withinSubjectsDesignChanged()));
	connect(_designTableModel, SIGNAL(factorAdded(Terms)), _withinSubjectsTermsModel, SLOT(addFixedFactors(Terms)));
	connect(_designTableModel, SIGNAL(factorRemoved(Terms)), _withinSubjectsTermsModel, SLOT(removeVariables(Terms)));

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

void AnovaRepeatedMeasuresShortForm::bindTo(Options *options, DataSet *dataSet)
{
	AnalysisForm::bindTo(options, dataSet);

	Terms factors;

	foreach (const Factor &factor, _designTableModel->design())
		factors.add(factor.first);

	_withinSubjectsTermsModel->setVariables(factors);

	if (_withinSubjectsTermsModel->terms().size() == 0)
		_withinSubjectsTermsModel->addFixedFactors(factors);

	_betweenSubjectsTermsModel->setVariables(_betweenSubjectsFactorsListModel->assigned());
}

void AnovaRepeatedMeasuresShortForm::factorsChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void AnovaRepeatedMeasuresShortForm::factorsChanged()
{
	Terms factorsAvailable;

	foreach (const Factor &factor, _designTableModel->design())
		factorsAvailable.add(factor.first);

	factorsAvailable.add(_betweenSubjectsFactorsListModel->assigned());

	_contrastsModel->setVariables(factorsAvailable);

	ui->postHocTests_variables->setVariables(factorsAvailable);

	if (_options != NULL)
		_options->blockSignals(false);
}

void AnovaRepeatedMeasuresShortForm::termsChanged()
{
	Terms terms;

	terms.add(string("~OVERALL"));

	foreach (const Factor &factor, _designTableModel->design())
		terms.add(factor.first);

	terms.add(_withinSubjectsTermsModel->terms());

	ui->marginalMeans_terms->setVariables(terms);
}

void AnovaRepeatedMeasuresShortForm::withinSubjectsDesignChanged()
{
	_withinSubjectCellsListModel->setDesign(_designTableModel->design());
	factorsChanged();
}

void AnovaRepeatedMeasuresShortForm::anovaDesignTableClicked(QModelIndex index)
{
	// the second column contains an X to delete the row

	if (index.column() == 1)
		_designTableModel->removeRow(index.row());
}
