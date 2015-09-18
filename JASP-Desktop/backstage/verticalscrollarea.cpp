
#include "verticalscrollarea.h"

#include <QEvent>
#include <QScrollBar>
#include <QResizeEvent>

VerticalScrollArea::VerticalScrollArea(QWidget *parent) : QScrollArea(parent)
{
	setWidgetResizable(true);
	setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
	setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
}

bool VerticalScrollArea::eventFilter(QObject *object, QEvent *event)
{
	// This works because QScrollArea::setWidget installs an eventFilter on the widget

	if(object && object == widget() && event->type() == QEvent::Resize)
		setMinimumWidth(widget()->minimumSizeHint().width() + verticalScrollBar()->width());

	return QScrollArea::eventFilter(object, event);
}

