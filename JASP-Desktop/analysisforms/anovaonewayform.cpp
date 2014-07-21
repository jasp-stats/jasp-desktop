#include "anovaonewayform.h"
#include "ui_anovaonewayform.h"

#include "analysisform.h"

AnovaOneWayForm::AnovaOneWayForm(QWidget *parent) :
	AnalysisForm("AnovaOneWayForm", parent),
	ui(new Ui::AnovaOneWayForm)
{
	ui->setupUi(this);

	ui->listAvailableFields->setModel(&_availableVariablesModel);
	ui->listAvailableFields->setDoubleClickTarget(ui->variables);

	_variablesModel.setSource(&_availableVariablesModel);
	_variablesModel.setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	_variablesModel.setVariableTypesSuggested(Column::ColumnTypeScale);
	ui->variables->setModel(&_variablesModel);
	ui->variables->setDoubleClickTarget(ui->listAvailableFields);

	_groupingVariableModel.setVariableTypesAllowed(Column::ColumnTypeNominalText | Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_groupingVariableModel.setVariableTypesSuggested(Column::ColumnTypeOrdinal | Column::ColumnTypeNominal);
	_groupingVariableModel.setSource(&_availableVariablesModel);
	ui->groupingVariable->setModel(&_groupingVariableModel);
	ui->groupingVariable->setDoubleClickTarget(ui->listAvailableFields);
	connect(&_groupingVariableModel, SIGNAL(assignmentsChanged()), this, SLOT(groupingVariableChanged()));

	ui->buttonAssignVariables->setSourceAndTarget(ui->listAvailableFields, ui->variables);
	ui->buttonAssignGroupingVariable->setSourceAndTarget(ui->listAvailableFields, ui->groupingVariable);

	ui->contrasts->setModel(&_contrastsModel);
	ui->contrasts->horizontalHeader()->setVisible(true);

	ui->panelContrasts->hide();
	ui->panelPostHoc->hide();
	ui->panelOptions->hide();

	connect(ui->contrasts, SIGNAL(clicked(QModelIndex)), this, SLOT(contrastsClicked(QModelIndex)));
}

AnovaOneWayForm::~AnovaOneWayForm()
{
	delete ui;
}

void AnovaOneWayForm::groupingVariableChanged()
{
	Terms terms = _groupingVariableModel.assigned();
	if (terms.size() > 0)
	{
		QVariant value = this->requestInfo(terms.at(0), VariableInfo::Labels);
		Terms labels(value.toStringList());
		_contrastsModel.setLabels(labels);
	}
	else
	{
		_contrastsModel.setLabels(Terms());
	}
}

void AnovaOneWayForm::contrastsClicked(QModelIndex index)
{
	if (index.column() == _contrastsModel.columnCount() - 1)
	{
		_contrastsModel.setData(index, "A");
	}
	else if (index.column() > 0)
	{
		QString value = _contrastsModel.data(index).toString();

		if (value == "")
			_contrastsModel.setData(index, "A");
		else if (value == "A")
			_contrastsModel.setData(index, "B");
		else
			_contrastsModel.setData(index, "");
	}
}
