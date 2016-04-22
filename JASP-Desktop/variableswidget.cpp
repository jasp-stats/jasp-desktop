
#include "variableswidget.h"
#include "ui_variableswidget.h"

#include "variablespage/variablestablemodel.h"

#include <QModelIndexList>
#include <boost/foreach.hpp>
#include <algorithm>
#include <QDebug>

VariablesWidget::VariablesWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::VariablesWidget)
{
	ui->setupUi(this);

	_dataSet = NULL;
	_currentColumn = NULL;

	_variablesTableModel = new VariablesTableModel(this);
	_levelsTableModel = new LevelsTableModel(this);

	ui->variablesList->setModel(_variablesTableModel);
	ui->levelsList->setModel(_levelsTableModel);
    /*
    ui->levelsList->setDragEnabled(true);
    ui->levelsList->setAcceptDrops(true);
    ui->levelsList->viewport()->setAcceptDrops(true);
    ui->levelsList->setDragDropOverwriteMode(false);
    ui->levelsList->setDropIndicatorShown(true);

    ui->levelsList->setSelectionMode(QAbstractItemView::SingleSelection);
    ui->levelsList->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui->levelsList->setDragDropMode(QAbstractItemView::InternalMove);
    */

	connect(ui->variablesList->selectionModel(), SIGNAL(currentRowChanged(QModelIndex,QModelIndex)), this, SLOT(selectedVariableChanged(QModelIndex,QModelIndex)));
	connect(ui->moveUpButton, SIGNAL(clicked()), this, SLOT(moveUpClicked()));
    connect(ui->moveDownButton, SIGNAL(clicked()), this, SLOT(moveDownClicked()));
    connect(ui->reverseButton, SIGNAL(clicked()), this, SLOT(reverseClicked()));
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
	_currentColumn = NULL;
	_variablesTableModel->clearDataSet();
}

void VariablesWidget::selectedVariableChanged(QModelIndex selection, QModelIndex old)
{
	Q_UNUSED(old);

	int columnIndex = selection.row();
	_currentColumn = &_dataSet->columns().at(columnIndex);

	_levelsTableModel->setColumn(_currentColumn);
}

void VariablesWidget::moveUpClicked()
{
	QModelIndexList selection = ui->levelsList->selectionModel()->selectedIndexes();

	if (selection.length() == 0)
			return;

	qSort(selection.begin(), selection.end(), qLess<QModelIndex>());

	if (selection.at(0).row() == 0) return;

	_levelsTableModel->moveUp(selection);

	// Reset Selection
	ui->levelsList->clearSelection();
	ui->levelsList->setSelectionMode(QAbstractItemView::MultiSelection);
	BOOST_FOREACH (QModelIndex &index, selection)
	{
		if (index.column() == 0) {
			ui->levelsList->selectRow(index.row() - 1);
		}
	}


}

void VariablesWidget::moveDownClicked()
{
	QModelIndexList selection = ui->levelsList->selectionModel()->selectedIndexes();
	if (selection.length() == 0)
		return;

	qSort(selection.begin(), selection.end(), qGreater<QModelIndex>());

	QModelIndex dummy = QModelIndex();
	if (selection.at(0).row() >= (_levelsTableModel->rowCount(dummy) - 1)) return;

	_levelsTableModel->moveDown(selection);

	// Reset Selection
	ui->levelsList->clearSelection();
	ui->levelsList->setSelectionMode(QAbstractItemView::MultiSelection);
	BOOST_FOREACH (QModelIndex &index, selection)
	{
		if (index.column() == 0) {
			ui->levelsList->selectRow(index.row() + 1);
		}
	}

}

void VariablesWidget::reverseClicked() {
    _levelsTableModel->reverse();
}
