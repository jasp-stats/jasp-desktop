#ifndef VERTICALTABWIDGET_H
#define VERTICALTABWIDGET_H

#include <QStackedWidget>
#include <QHBoxLayout>

#include "verticaltabbar.h"

class VerticalTabWidget : public QWidget
{
public:
	VerticalTabWidget(QWidget *parent = NULL);

	void addTab(QWidget *page, const QString &label, const QIcon &icon = QIcon());
	void hideTab(QWidget *page);
	void showTab(QWidget *page);
	void setTabEnabled(int index, bool enable);

	void setTabStyleSheet(const QString &styleSheet);

	VerticalTabBar *tabBar();
	QWidget *container();

private:
	QHBoxLayout *_layout;
	VerticalTabBar *_tabBar;
	QStackedWidget *_stackWidget;
};

#endif // VERTICALTABWIDGET_H
