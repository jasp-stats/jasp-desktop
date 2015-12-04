
#include "variableswidget.h"
#include "ui_variableswidget.h"

#include "variablespage/variablestablemodel.h"

#include <QModelIndexList>

VariablesWidget::VariablesWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::VariablesWidget)
{
	ui->setupUi(this);

	_dataSet = NULL;

	_variablesTableModel = new VariablesTableModel(this);
	_levelsTableModel = new LevelsTableModel(this);

	ui->variablesList->setModel(_variablesTableModel);
	ui->levelsList->setModel(_levelsTableModel);

	connect(ui->variablesList->selectionModel(), SIGNAL(currentRowChanged(QModelIndex,QModelIndex)), this, SLOT(selectedVariableChanged(QModelIndex,QModelIndex)));
}

VariablesWidget::~VariablesWidget()
{
	delete ui;
}

void VariablesWidget::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;
	_variablesTableModel->setDataSet(dataSet);
}

void VariablesWidget::clearDataSet()
{
	_dataSet = NULL;
	_variablesTableModel->clearDataSet();
}

void VariablesWidget::selectedVariableChanged(QModelIndex selection, QModelIndex old)
{
	Q_UNUSED(old);

	int columnIndex = selection.row();
	Column &column = _dataSet->columns().at(columnIndex);

	_levelsTableModel->setColumn(&column);
}

void VariablesWidget::moveUpClicked()
{
	QModelIndexList selection = ui->levelsList->selectionModel()->selectedIndexes();
	if (selection.length() == 0)
		return;


}

void VariablesWidget::moveDownClicked()
{
	QModelIndexList selection = ui->levelsList->selectionModel()->selectedIndexes();
	if (selection.length() == 0)
		return;


}
