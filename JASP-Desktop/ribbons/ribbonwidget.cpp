#include "ribbonwidget.h"

#include <QDebug>
#include <QMenu>
#include "widgets/ribbonbutton.h"

RibbonWidget::RibbonWidget(QWidget *parent) :
	QWidget(parent)
{
}

void RibbonWidget::itemSelected()
{
	QObject *source = this->sender();
	QString name = source->objectName();

	emit itemSelected(name);
}

void RibbonWidget::menuHiding()
{
	QMenu *menu = qobject_cast<QMenu *>(this->sender());
	RibbonButton *button = qobject_cast<RibbonButton *>(menu->parent());

	qDebug() << button;
}


