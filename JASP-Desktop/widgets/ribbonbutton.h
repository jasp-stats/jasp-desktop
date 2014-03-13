#ifndef RIBBONBUTTON_H
#define RIBBONBUTTON_H

#include <QToolButton>
#include <QString>
#include <QMouseEvent>

#include "common.h"

class RibbonButton : public QToolButton
{
    Q_OBJECT
public:
	explicit RibbonButton(QWidget *parent = 0);

public slots:
	void notifyMouseOut();
	void notifyMouseOver();

protected:
	virtual void enterEvent(QEvent *event) OVERRIDE;
	virtual void mousePressEvent(QMouseEvent *event) OVERRIDE;

private:

	bool _mouseOver;

	QString _mouseOutSS;
	QString _mouseOverSS;

	bool _connectedToMenu;
};

#endif // RIBBONBUTTON_H
