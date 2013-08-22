#include "tableview.h"

#include <QLabel>
#include <QGridLayout>
#include <QHeaderView>

#include "datasettablemodel.h"

TableView::TableView(QWidget *parent) :
	QTableView(parent)
{
	_infoPopup = new InfoPopup(this);
	_infoPopup->setVisible(false);
	_infoPopupVisible = false;

	QLabel *label = new QLabel(QString("This column only permits numeric values"), _infoPopup);
	label->setWordWrap(true);
	_infoPopup->layout()->addWidget(label);
}

void TableView::setModel(QAbstractItemModel *model)
{
	DataSetTableModel *dataSetModel = dynamic_cast<DataSetTableModel *>(model);

	if (dataSetModel != NULL)
		connect(dataSetModel, SIGNAL(badDataEntered(QModelIndex)), this, SLOT(badDataEnteredHandler(QModelIndex)));

	QTableView::setModel(model);
}

void TableView::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	hideInfoPopup();

	QTableView::selectionChanged(selected, deselected);
}

void TableView::verticalScrollbarValueChanged(int value)
{
	if (_infoPopupVisible)
	{
		moveInfoPopup();
	}
}

void TableView::badDataEnteredHandler(QModelIndex index)
{
	showInfoPopup(index);
}

void TableView::showInfoPopup(QModelIndex &index)
{
	_infoPopupIndex = index;
	_infoPopupVisible = true;
	moveInfoPopup();
}

void TableView::moveInfoPopup()
{
	/*int headerWidth = this->verticalHeader()->width();
	int headerHeight = this->horizontalHeader()->height();

	QRect rect = this->visualRect(index);
	_infoPopup->move(headerWidth + rect.x() +  _infoPopup->pointLength() / 2, headerHeight + rect.y() + rect.height() - _infoPopup->pointLength() / 2);
	_infoPopup->setVisible(true);
	_infoPopupVisible = true;*/

	int headerWidth = this->verticalHeader()->width();
	int headerHeight = this->horizontalHeader()->height();

	QRect rect = this->visualRect(_infoPopupIndex);
	QRect visibleArea = this->rect();

	if (visibleArea.contains(rect))
	{
		_infoPopup->show();

		int top = headerHeight + rect.y() + rect.height() - _infoPopup->pointLength() / 2;
		int left = headerWidth + rect.x() +  _infoPopup->pointLength() / 2;

		int bottom = top + _infoPopup->height();

		int visibleBottom = this->viewport()->height();

		if (bottom > visibleBottom)
		{
			_infoPopup->setDirection(InfoPopup::BottomLeft);
			top = headerHeight + rect.y() - _infoPopup->height() + _infoPopup->pointLength() / 2 + 4; // don't know why +4
		}
		else
		{
			_infoPopup->setDirection(InfoPopup::TopLeft);
		}

		_infoPopup->move(left, top);
	}
	else
	{
		_infoPopup->hide();
	}
}

void TableView::hideInfoPopup()
{
	if (_infoPopupVisible)
	{
		_infoPopup->hide();
		_infoPopupVisible = false;
	}
}
