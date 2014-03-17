#ifndef MAINTABBAR_H
#define MAINTABBAR_H

#include <QWidget>
#include <QTabBar>
#include <QGridLayout>
#include <QMouseEvent>
#include <QFocusEvent>

#include "common.h"

class MainTabBar : public QWidget
{
	Q_OBJECT

public:
	explicit MainTabBar(QWidget *parent = 0);

	enum Side { Left, Right };

	void addTab(QString tabName, Side side = Left);
	void setCurrentIndex(int index);

private:

	class TabBar : public QTabBar
	{
	public:
		TabBar(MainTabBar *parent) : QTabBar(parent)
		{
			_parent = parent;
			setFocusPolicy(Qt::ClickFocus);
		}

	protected:

		virtual void focusInEvent(QFocusEvent *) OVERRIDE
		{
			_parent->setActive(this);
		}

	private:
		MainTabBar *_parent;
	};


signals:
	void currentChanged(int index);

private slots:
	void tabChanged();

protected:
	virtual void mousePressEvent(QMouseEvent *event);

private:

	QString _tabStylesheet;
	QString _tabStylesheetActive;

	void setActive(Side side);
	void setActive(TabBar *tabBar);

	Side _active = Left;

	QGridLayout *_layout;
	TabBar *_tabsLeft;
	TabBar *_tabsRight;
	QWidget *_spacer;

};

#endif // MAINTABBAR_H
