#ifndef RIBBONWIDGET_H
#define RIBBONWIDGET_H

#include <QWidget>

class RibbonWidget : public QWidget
{
	Q_OBJECT
public:
	explicit RibbonWidget(QWidget *parent = 0);

signals:
	void itemSelected(QString itemName);

protected slots:
	void itemSelected();
	void menuItemSelected();

};

#endif // RIBBONWIDGET_H
