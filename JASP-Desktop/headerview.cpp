#include "headerview.h"

#include <QPainter>
#include <QDebug>
#include "dataset.h"

HeaderView::HeaderView(Qt::Orientation orientation, QWidget *parent) :
	QHeaderView(orientation, parent)
{
}

void HeaderView::paintSection(QPainter *painter, const QRect &rect, int logicalIndex) const
{
	if ( ! rect.isValid())
		return;

	painter->save();
	QHeaderView::paintSection(painter, rect, logicalIndex);
	painter->restore();

	static QPixmap ordinal = QPixmap(":/icons/variable-ordinal.png").scaled(16, 16);
	static QPixmap nominal = QPixmap(":/icons/variable-nominal.png").scaled(16, 16);
	static QPixmap scale = QPixmap(":/icons/variable-scale.png").scaled(16, 16);

	QRect r(rect.x() + 4, rect.y() + 4, 16, 16);

	QVariant v = this->model()->headerData(logicalIndex, this->orientation(), Qt::UserRole);

	if (v == Column::ColumnTypeNominal)
		painter->drawPixmap(r, nominal);
	else if (v == Column::ColumnTypeOrdinal)
		painter->drawPixmap(r, ordinal);
	else
		painter->drawPixmap(r, scale);
}
