
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

	_levelsTableModel = new LevelsTableModel(this);

	ui->labelsView->setModel(_levelsTableModel);
	ui->columnheader->setText("");
	(ui->labelsView->verticalHeader())->hide();

	setToolTip("Double-click on a label to change it");

	connect(ui->moveUpButton, SIGNAL(clicked()), this, SLOT(moveUpClicked()));
    connect(ui->moveDownButton, SIGNAL(clicked()), this, SLOT(moveDownClicked()));
	connect(ui->reverseButton, SIGNAL(clicked()), this, SLOT(reverseClicked()));
	connect(ui->closeButton, SIGNAL(clicked()), this, SLOT(close()));
	connect(_levelsTableModel, SIGNAL(dataChanged(QModelIndex, QModelIndex)), this, SLOT(labelDataChanged(QModelIndex, QModelIndex)));

}

VariablesWidget::~VariablesWidget()
{
	delete ui;
}

void VariablesWidget::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;
	_levelsTableModel->refresh();
}

void VariablesWidget::clearDataSet()
{
	_dataSet = NULL;
	_currentColumn = NULL;
}

void VariablesWidget::moveUpClicked()
{
	QModelIndexList selection = ui->labelsView->selectionModel()->selectedIndexes();

	if (selection.length() == 0)
			return;

	qSort(selection.begin(), selection.end(), qLess<QModelIndex>());

	if (selection.at(0).row() == 0) return;

	_levelsTableModel->moveUp(selection);

	// Reset Selection
	ui->labelsView->clearSelection();
	ui->labelsView->setSelectionMode(QAbstractItemView::MultiSelection);

	BOOST_FOREACH (QModelIndex &index, selection)
	{
		if (index.column() == 0) {
			ui->labelsView->selectRow(index.row() - 1);
		}
	}

	//ui->labelsView->setSelectionBehavior(QAbstractItemView::SelectRows);
	ui->labelsView->setSelectionMode(QAbstractItemView::ExtendedSelection);

	if (_currentColumn != NULL)
		emit(columnChanged(QString::fromStdString(_currentColumn->name())));

}

void VariablesWidget::moveDownClicked()
{
	QModelIndexList selection = ui->labelsView->selectionModel()->selectedIndexes();
	if (selection.length() == 0)
		return;

	qSort(selection.begin(), selection.end(), qGreater<QModelIndex>());

	QModelIndex dummy = QModelIndex();
	if (selection.at(0).row() >= (_levelsTableModel->rowCount(dummy) - 1)) return;

	_levelsTableModel->moveDown(selection);

	// Reset Selection
	ui->labelsView->clearSelection();
	ui->labelsView->setSelectionMode(QAbstractItemView::MultiSelection);

	BOOST_FOREACH (QModelIndex &index, selection)
	{
		if (index.column() == 0) {
			ui->labelsView->selectRow(index.row() + 1);
		}
	}

	//ui->labelsView->setSelectionBehavior(QAbstractItemView::SelectRows);
	ui->labelsView->setSelectionMode(QAbstractItemView::ExtendedSelection);

	if (_currentColumn != NULL)
		emit(columnChanged(QString::fromStdString(_currentColumn->name())));

}

void VariablesWidget::reverseClicked()
{
	_levelsTableModel->reverse();

	ui->labelsView->setSelectionBehavior(QAbstractItemView::SelectRows);
	ui->labelsView->setSelectionMode(QAbstractItemView::ExtendedSelection);

	if (_currentColumn != NULL)
		emit(columnChanged(QString::fromStdString(_currentColumn->name())));
}

void VariablesWidget::setCurrentColumn(int columnnumber)
{
	_currentColumn = &_dataSet->columns().at(columnnumber);

	QString columnheader(_currentColumn->name().c_str());

	_levelsTableModel->setColumn(_currentColumn);

	ui->columnheader->setText("<b>" + columnheader + "</b>");

}

void VariablesWidget::labelDataChanged(QModelIndex m1, QModelIndex m2)
{
	if (_currentColumn != NULL)
		emit(columnChanged(QString::fromStdString(_currentColumn->name())));
	emit(resetTableView());
}
