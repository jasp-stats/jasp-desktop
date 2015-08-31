#ifndef VERTICALSCROLLAREA_H
#define VERTICALSCROLLAREA_H

#include <QScrollArea>

#include <QResizeEvent>

#include "common.h"

class VerticalScrollArea : public QScrollArea
{
	Q_OBJECT
public:
	explicit VerticalScrollArea(QWidget *parent = 0);

protected:
	bool eventFilter(QObject *object, QEvent *event) OVERRIDE;
};

#endif // VERTICALSCROLLAREA_H
