#ifndef TABLEVIEWMENUEDITOR_H
#define TABLEVIEWMENUEDITOR_H

#include <QWidget>
#include <QMenu>

#include "common.h"

class TableViewMenuEditor : public QWidget
{
	Q_OBJECT
public:
	explicit TableViewMenuEditor(QWidget *parent = 0);
	QString selected();

	void setMenuContent(QStringList menuItems, QString selected);

signals:
	void editingFinished();

protected:
	virtual void showEvent(QShowEvent *) OVERRIDE;
	virtual void paintEvent(QPaintEvent *event) OVERRIDE;

private slots:
	void itemSelected(QAction *action);

private:

	QMenu *_menu;
	QString _selected;

};

#endif // TABLEVIEWMENUEDITOR_H
