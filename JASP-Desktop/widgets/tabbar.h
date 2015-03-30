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
	void removeTab(QString tabName);
	void removeTab(int index);

	void addOptionsTab();
	void addHelpTab();

	int count() const;

signals:
	void currentChanged(int index);
	void helpToggled(bool on);

public slots:
	void setCurrentIndex(int index);

private slots:
	void tabSelectedHandler();
	void helpToggledHandler(bool on);

private:

	QWidget *_background;
	QList<QPushButton *> _tabButtons;
	QGridLayout *_backgroundLayout;
	QHBoxLayout *_layout;

	QPushButton *_optionsTab;
	QPushButton *_helpTab;

};

#endif // TABBAR_H
