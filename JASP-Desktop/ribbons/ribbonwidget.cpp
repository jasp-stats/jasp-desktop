#include "ribbonwidget.h"

#include <QPushButton>

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

void RibbonWidget::menuItemSelected()
{
	QPushButton *source = dynamic_cast<QPushButton*>(this->sender());
	if (source != NULL)
		source->showMenu();
}

