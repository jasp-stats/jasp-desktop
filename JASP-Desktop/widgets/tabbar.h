#ifndef TABBAR_H
#define TABBAR_H

#include <QWidget>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QList>
#include <QPushButton>

class TabBar : public QWidget
{
	Q_OBJECT
public:
	explicit TabBar(QWidget *parent = 0);

	void addTab(QString tabName);
	void addLastTab(QString tabName);
	void removeTab(int index);
	int count() const;

signals:
	void currentChanged(int index);

public slots:
	void setCurrentIndex(int index);

private slots:
	void tabSelectedHandler();

private:

	QWidget *_background;
	QList<QPushButton *> _tabButtons;
	QGridLayout *_backgroundLayout;
	QHBoxLayout *_layout;

	QPushButton *_lastTab;

};

#endif // TABBAR_H
