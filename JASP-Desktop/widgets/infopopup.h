#ifndef INFOPOPUP_H
#define INFOPOPUP_H

#include <QWidget>
#include <QPainterPath>
#include <QPen>

#include "common.h"

class InfoPopup : public QWidget
{
	Q_OBJECT
public:
	explicit InfoPopup(QWidget *parent = 0);

	enum PopupDirection { BottomRight, BottomLeft, TopLeft };

	void setDirection(PopupDirection direction);

	int pointLength();
	int pointWidth();
	int pointOffset();
	
signals:

protected:
	virtual void paintEvent(QPaintEvent *event) OVERRIDE;
	
public slots:

private:
	PopupDirection _popupDirection;
	QBrush _backgroundBrush;
	QPen _borderPen;

	int _pointLength;
	int _pointWidth;
	int _pointOffset;
	int _borderWidth;
	
};

#endif // INFOPOPUP_H
