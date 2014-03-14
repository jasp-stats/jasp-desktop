#include "boundpairstable.h"
#include "QDebug"
#include <QHeaderView>

#include <boost/foreach.hpp>

#include <QHBoxLayout>
#include <QLabel>

#include "tablemodelvariablesassigned.h"

using namespace std;

BoundPairsTable::BoundPairsTable(QWidget *parent) :
	TableView(parent)
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

void BoundPairsTable::notifyDragWasDropped()
{
	if (_tableModel != NULL)
		_tableModel->mimeDataMoved(selectedIndexes());
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
			QIcon icon(":/icons/variable-nominal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_tableModel->variableTypesAllowed() & Column::ColumnTypeOrdinal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-ordinal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_tableModel->variableTypesAllowed() & Column::ColumnTypeScale)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-scale-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		_variableTypeKey->resize(_variableTypeKey->sizeHint());
	}

	repositionKey();
	_variableTypeKey->resize(_variableTypeKey->sizeHint());
}
