#include "boundpairstable.h"
#include "QDebug"
#include <QHeaderView>

#include <boost/foreach.hpp>

#include <QHBoxLayout>
#include <QLabel>

#include "tablemodelvariablesassigned.h"

using namespace std;

BoundPairsTable::BoundPairsTable(QWidget *parent) :
	QTableView(parent)
{
	_variableTypeKey = NULL;

	setModel(new TableModelVariablesAssigned(this));
	setSelectionMode(QAbstractItemView::ContiguousSelection);

	this->setDragEnabled(true);
	this->viewport()->setAcceptDrops(true);
	this->setDropIndicatorShown(true);
	this->setDragDropMode(QAbstractItemView::DragDrop);

	horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
	horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
	horizontalHeader()->setSectionsClickable(false);
	horizontalHeader()->hide();

	verticalHeader()->hide();

	setupKey();
}

void BoundPairsTable::bindTo(Option *option)
{
	if (_tableModel != NULL)
		_tableModel->bindTo(option);
}

void BoundPairsTable::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	QTableView::selectionChanged(selected, deselected);

	QModelIndexList indices = selectedIndexes();

	if (indices.length() > 1)
	{
		QModelIndex first = indices.first();
		QModelIndex last = indices.first();

		BOOST_FOREACH(QModelIndex &index, indices)
		{
			if (index.row() <= first.row() && index.column() <= first.column())
				first = index;
			if (index.row() >= last.row() && index.column() >= last.column())
				last = index;
		}

		int rowsSelected = last.row() - first.row() + 1;
		int colsSelected = last.column() - first.column() + 1;

		if (rowsSelected > 1 && colsSelected < 2)
		{
			first = model()->index(first.row(), 0);
			last = model()->index(last.row(), 1);

			QItemSelection selection;
			selection.select(first, last);

			this->selectionModel()->select(selection, QItemSelectionModel::ClearAndSelect);
		}
	}

	emit selectionUpdated();
}

void BoundPairsTable::startDrag(Qt::DropActions supportedActions)
{
	QModelIndexList indices = selectedIndexes();

	QModelIndex first = indices.first();
	QModelIndex last = indices.first();

	foreach(QModelIndex index, indices)
	{
		if (index.row() <= first.row() && index.column() <= first.column())
			first = index;
		if (index.row() >= last.row() && index.column() >= last.column())
			last = index;
	}

	int rowsSelected = last.row() - first.row() + 1;
	int colsSelected = last.column() - first.column() + 1;

	if (colsSelected < 2)
	{
		first = model()->index(first.row(), 0);
		last = model()->index(last.row(), 1);

		QItemSelection selection;
		selection.select(first, last);

		this->selectionModel()->select(selection, QItemSelectionModel::ClearAndSelect);
	}

	QTableView::startDrag(supportedActions);
}

void BoundPairsTable::focusInEvent(QFocusEvent *event)
{
	emit focused();
}

void BoundPairsTable::setModel(QAbstractItemModel *model)
{
	_tableModel = qobject_cast<TableModelVariablesAssigned *>(model);

	setupKey();

	QTableView::setModel(model);
}

void BoundPairsTable::resizeEvent(QResizeEvent *e)
{
	QTableView::resizeEvent(e);

	repositionKey();
}

void BoundPairsTable::moveEvent(QMoveEvent *e)
{
	QTableView::moveEvent(e);

	repositionKey();
}

void BoundPairsTable::repositionKey()
{
	_variableTypeKey->move(this->width() - _variableTypeKey->width(), this->height() - _variableTypeKey->height());
}

void BoundPairsTable::setupKey()
{
	if (_variableTypeKey != NULL)
		delete _variableTypeKey;

	_variableTypeKey = new QWidget(this);
	QHBoxLayout *layout = new QHBoxLayout(_variableTypeKey);
	layout->setSpacing(4);
	layout->setContentsMargins(4, 4, 4, 4);
	_variableTypeKey->setLayout(layout);

	if (_tableModel != NULL)
	{
		if (_tableModel->variableTypesAllowed() & Column::ColumnTypeNominal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-nominal-trans.png");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_tableModel->variableTypesAllowed() & Column::ColumnTypeOrdinal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-ordinal-trans.png");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_tableModel->variableTypesAllowed() & Column::ColumnTypeScale)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-scale-trans.png");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		_variableTypeKey->resize(_variableTypeKey->sizeHint());
	}

	repositionKey();
	_variableTypeKey->resize(_variableTypeKey->sizeHint());
}
