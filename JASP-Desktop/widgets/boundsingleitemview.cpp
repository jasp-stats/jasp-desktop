#include "boundsingleitemview.h"
#include "boundlistview.h"



boundSingleItemView::boundSingleItemView(QWidget *parent)
	: BoundListView(parent)
{
	/*int height = this->fontMetrics().height();
	this->setMaximumHeight(height);
	this->setMinimumHeight(height);*/
}

int boundSingleItemView::itemCount() const
{
	return 1;
}

QSize boundSingleItemView::sizeHint() const
{
	static int height = -1;

	if (height == -1)
		height = this->fontMetrics().height() + 10;

	QSize sizeHint = BoundListView::sizeHint();
	sizeHint.setHeight(height);

	return sizeHint;
}

QSize boundSingleItemView::minimumSizeHint() const
{
	static int height = -1;

	if (height == -1)
		height = this->fontMetrics().height() + 10;

	QSize sizeHint = BoundListView::minimumSizeHint();
	sizeHint.setHeight(height);

	return sizeHint;
}
