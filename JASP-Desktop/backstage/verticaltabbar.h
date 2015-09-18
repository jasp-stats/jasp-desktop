#ifndef VERTICALTABBAR_H
#define VERTICALTABBAR_H

#include <QWidget>
#include <QVBoxLayout>
#include <QIcon>
#include <QButtonGroup>
#include <QPushButton>

#include "common.h"

class VerticalTabBar : public QWidget
{
	Q_OBJECT
public:
	explicit VerticalTabBar(QWidget *parent = 0);

	void addTab(const QString &label, const QIcon &icon = QIcon());
	void hideTab(int index);
	void showTab(int index);
	bool isTabVisible(int index);
	void setTabEnabled(int index, bool enable);

	void setCurrentIndex(int index);
	int currentIndex() const;
	int count() const;

protected:
	void paintEvent(QPaintEvent *) OVERRIDE;

signals:
	void currentChanging(int newIndex, bool &cancel);
	void currentChanged(int index);

private slots:

	void buttonClicked(int id);

private:
	int _selectedIndex;
	QVBoxLayout *_layout;
	QButtonGroup *_buttonGroup;
};

#endif // VERTICALTABBAR_H
