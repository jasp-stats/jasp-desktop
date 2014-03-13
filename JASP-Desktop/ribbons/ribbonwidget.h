#ifndef RIBBONWIDGET_H
#define RIBBONWIDGET_H

#include <QWidget>
#include "widgets/ribbonbutton.h"

class RibbonWidget : public QWidget
{
	Q_OBJECT
public:
	explicit RibbonWidget(QWidget *parent = 0);

signals:
	void itemSelected(QString itemName);

protected slots:
	void itemSelected();
	void menuHiding();

};

#endif // RIBBONWIDGET_H
