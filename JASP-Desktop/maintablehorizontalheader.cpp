
#include "maintablehorizontalheader.h"

#include <QMouseEvent>
#include <QDebug>

MainTableHorizontalHeader::MainTableHorizontalHeader(QWidget *parent) :
	QHeaderView(Qt::Horizontal, parent)
{
	_menu = new QMenu(this);
	_columnSelected = 0;

	_nominalIcon = QIcon(":/icons/variable-nominal.png");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.png");
	_scaleIcon = QIcon(":/icons/variable-scale.png");

	_convertToScale = _menu->addAction(_scaleIcon, "", this, SLOT(scaleSelected()));
	_convertToOrdinal = _menu->addAction(_ordinalIcon, "", this, SLOT(ordinalSelected()));
	_convertToNominal = _menu->addAction(_nominalIcon, "", this, SLOT(nominalSelected()));

}

void MainTableHorizontalHeader::mousePressEvent(QMouseEvent *event)
{
	QPoint pos = event->pos();
	int index = logicalIndexAt(pos);

	int itemPos = sectionViewportPosition(index);

	int x = pos.x() - itemPos;

	if (x >= 4 && x <= 24)
	{
		_columnSelected = index;

		int columnTypesAllowed = model()->headerData(index, Qt::Horizontal, Qt::UserRole).toInt();

		_convertToNominal->setEnabled(columnTypesAllowed & Column::ColumnTypeNominal);
		_convertToOrdinal->setEnabled(columnTypesAllowed & Column::ColumnTypeOrdinal);
		_convertToScale->setEnabled(columnTypesAllowed & Column::ColumnTypeScale);

		QPoint menuPos = this->mapToGlobal(QPoint(itemPos, this->height()));

		_menu->move(menuPos);
		_menu->show();
	}


	QHeaderView::mousePressEvent(event);
}

void MainTableHorizontalHeader::nominalSelected()
{
	emit columnTypeChanged(_columnSelected, Column::ColumnTypeNominal);
}

void MainTableHorizontalHeader::ordinalSelected()
{
	emit columnTypeChanged(_columnSelected, Column::ColumnTypeOrdinal);
}

void MainTableHorizontalHeader::scaleSelected()
{
	emit columnTypeChanged(_columnSelected, Column::ColumnTypeScale);
}
