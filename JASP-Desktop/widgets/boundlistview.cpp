#include "boundlistview.h"

#include "boost/bind.hpp"
#include "boost/foreach.hpp"

#include <vector>
#include <string>

#include "options/optionvariables.h"

#include "widgets/boundmodel.h"
#include "tableviewmenueditordelegate.h"

#include <QLabel>
#include <QHBoxLayout>

using namespace std;

BoundListView::BoundListView(QWidget *parent)
	: ListView(parent)
{
	_variablesListModel = NULL;

	setEditTriggers(QListView::NoEditTriggers);
	setSelectionMode(QAbstractItemView::ExtendedSelection);

	this->setDragEnabled(true);
	this->viewport()->setAcceptDrops(true);
	this->setDropIndicatorShown(true);
	this->setDragDropMode(QAbstractItemView::DragDrop);

	_variableTypeKey = new QWidget(this);
	QHBoxLayout *layout = new QHBoxLayout(_variableTypeKey);
	layout->setSpacing(4);
	layout->setContentsMargins(4, 4, 4, 4);
	_variableTypeKey->setLayout(layout);
	_variableTypeKey->resize(_variableTypeKey->sizeHint());

	this->setItemDelegate(new TableViewMenuEditorDelegate(this));
}

void BoundListView::setModel(QAbstractItemModel *model)
{
	_variablesListModel = qobject_cast<TableModelVariablesAssigned *>(model);

	if (_variablesListModel != NULL)
	{
		if (_variablesListModel->variableTypesSuggested() & Column::ColumnTypeNominal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-nominal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_variablesListModel->variableTypesSuggested() & Column::ColumnTypeOrdinal)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-ordinal-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		if (_variablesListModel->variableTypesSuggested() & Column::ColumnTypeScale)
		{
			QLabel *label = new QLabel(_variableTypeKey);
			QIcon icon(":/icons/variable-scale-inactive.svg");
			QPixmap pixmap = icon.pixmap(16, 16);
			label->setPixmap(pixmap);
			_variableTypeKey->layout()->addWidget(label);
		}

		_variableTypeKey->resize(_variableTypeKey->sizeHint());

		repositionKey();
	}

	ListView::setModel(model);
}

void BoundListView::bindTo(Option *option)
{
	BoundModel *model = dynamic_cast<BoundModel *>(this->model());
	if (model != NULL)
		model->bindTo(option);
}

void BoundListView::unbind()
{
	BoundModel *model = dynamic_cast<BoundModel *>(this->model());
	if (model != NULL)
		model->unbind();
}

void BoundListView::resizeEvent(QResizeEvent *e)
{
	ListView::resizeEvent(e);

	repositionKey();
}

void BoundListView::moveEvent(QMoveEvent *e)
{
	ListView::moveEvent(e);

	repositionKey();
}

void BoundListView::repositionKey()
{
	_variableTypeKey->move(this->width() - _variableTypeKey->width(), this->height() - _variableTypeKey->height());
}

