
#include "verticaltabwidget.h"

VerticalTabWidget::VerticalTabWidget(QWidget *parent) : QWidget(parent)
{
	_layout = new QHBoxLayout(this);
	_layout->setContentsMargins(0, 0, 0, 0);
	_layout->setSpacing(0);

	_tabBar = new VerticalTabBar(this);
	_stackWidget = new QStackedWidget(this);
	_stackWidget->layout()->setContentsMargins(0, 0, 0, 0);

	_layout->addWidget(_tabBar);
	_layout->addWidget(_stackWidget, 1);

	connect(_tabBar, SIGNAL(currentChanged(int)), _stackWidget, SLOT(setCurrentIndex(int)));
}

void VerticalTabWidget::addTab(QWidget *page, const QString &label, const QIcon &icon)
{
	_tabBar->addTab(label, icon);
	_stackWidget->addWidget(page);
}

void VerticalTabWidget::showTab(QWidget *page)
{
	int index = _stackWidget->indexOf(page);
	_tabBar->showTab(index);
}

void VerticalTabWidget::hideTab(QWidget *page)
{
	int index = _stackWidget->indexOf(page);
	_tabBar->hideTab(index);
}

void VerticalTabWidget::setTabEnabled(int index, bool enable)
{
	_stackWidget->widget(index)->setEnabled(enable);
	_tabBar->setTabEnabled(index, enable);
}

VerticalTabBar *VerticalTabWidget::tabBar()
{
	return _tabBar;
}

QWidget *VerticalTabWidget::container()
{
	return _stackWidget;
}

