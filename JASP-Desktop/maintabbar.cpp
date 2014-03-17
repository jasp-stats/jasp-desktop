#include "maintabbar.h"

#include <QDebug>

MainTabBar::MainTabBar(QWidget *parent) :
	QWidget(parent)
{
	_tabsLeft = new TabBar(this);
	_tabsRight = new TabBar(this);
	_spacer = new QWidget(this);

	connect(_tabsLeft, SIGNAL(currentChanged(int)), this, SLOT(tabChanged()));
	connect(_tabsRight, SIGNAL(currentChanged(int)), this, SLOT(tabChanged()));

	QString spacerStylesheet(
			"QWidget {"
			"  border-bottom: 1.2px solid #C4C4C3 ;"
			"  background-color: #E8E8E8 ;"
			"}");

	_tabStylesheetActive = QString(
			"QTabBar {"
			"  border-bottom: 1.2px solid #C4C4C3 ;"
			"  background-color: #E8E8E8 ;"
			"}"
			"QTabBar::tab {"
			"  border-top-left-radius: 3px ;"
			"  border-top-right-radius: 3px ;"
			"  min-width: 6ex ;"
			"  padding: 4px 16px ;"
			"  margin: 4px 1.5px 0 1.5px ;"
			"}"
			""
			"QTabBar::tab:selected {"
			"  border: 1.2px solid #C4C4C3 ;"
			"  border-bottom: 1.2px solid white ;"
			"  background-color: white ;"
			"}"
			""
			"QTabBar::tab:first {"
			"  margin-left: 3px ;"
			"  color: white ;"
			"  border: 1.2px solid #1e47a0 ;"
			"  background: qradialgradient(cx: 0.5, cy: 2, radius: 2, fx: 0.5, fy: 1, stop: 0 #619ed7, stop: 0.4 #2561b7, stop: 0.6 #2561b7, stop: 1.0 #619ed7) ;"
			"}"
			""
			"QTabBar::tab:first:selected {"
			"  border-bottom: 1.2px solid #619ed7;"
			"  border: 1.2px solid #1e47a0;"
			"  background: qradialgradient(cx: 0.5, cy: -1, radius: 2, fx: 0.5, fy: 0, stop: 0 #619ed7, stop: 0.4 #2561b7, stop: 0.6 #2561b7, stop: 1.0 #619ed7) ;"
			"}");

	_tabStylesheet = QString(
			"QTabBar {"
			"  border-bottom: 1.2px solid #C4C4C3 ;"
			"  background-color: #E8E8E8 ;"
			"}"
			"QTabBar::tab {"
			"  border-top-left-radius: 3px ;"
			"  border-top-right-radius: 3px ;"
			"  min-width: 6ex ;"
			"  padding: 4px 16px ;"
			"  margin: 4px 1.5px 0 1.5px ;"
			"}"
			""
			"QTabBar::tab:first {"
			"  margin-left: 3px ;"
			"  color: white ;"
			"  border: 1.2px solid #1e47a0 ;"
			"  background: qradialgradient(cx: 0.5, cy: 2, radius: 2, fx: 0.5, fy: 1, stop: 0 #619ed7, stop: 0.4 #2561b7, stop: 0.6 #2561b7, stop: 1.0 #619ed7) ;"
			"}");

	_spacer->setStyleSheet(spacerStylesheet);
	setActive(Left);

	_layout = new QGridLayout(this);
	_layout->setContentsMargins(0, 0, 0, 0);
	_layout->setSpacing(0);

	this->setLayout(_layout);

	_layout->addWidget(_tabsLeft, 0, 0);
	_layout->addWidget(_spacer, 0, 1);
	_layout->addWidget(_tabsRight, 0, 2);

	_layout->setColumnStretch(1, 1);

}

void MainTabBar::addTab(QString tabName, MainTabBar::Side side)
{
	if (side == MainTabBar::Left)
		_tabsLeft->addTab(tabName);
	else
		_tabsRight->addTab(tabName);
}

void MainTabBar::setCurrentIndex(int index)
{
	int leftCount = _tabsLeft->count();

	if (index < leftCount)
	{
		_tabsLeft->setCurrentIndex(index);
		setActive(MainTabBar::Left);
	}
	else
	{
		_tabsRight->setCurrentIndex(index - leftCount);
		setActive(MainTabBar::Right);
	}
}

void MainTabBar::tabChanged()
{
	if (this->sender() == _tabsLeft)
		setActive(Left);
	else
		setActive(Right);
}

void MainTabBar::mousePressEvent(QMouseEvent *event)
{
	qDebug() << event;
}

void MainTabBar::setActive(TabBar *tabBar)
{
	if (tabBar == _tabsLeft)
		setActive(Left);
	else
		setActive(Right);
}

void MainTabBar::setActive(MainTabBar::Side side)
{
	_active = side;

	if (side == MainTabBar::Left)
	{
		_tabsLeft->setStyleSheet(_tabStylesheetActive);
		_tabsRight->setStyleSheet(_tabStylesheet);
	}
	else
	{
		_tabsLeft->setStyleSheet(_tabStylesheet);
		_tabsRight->setStyleSheet(_tabStylesheetActive);
	}
}
