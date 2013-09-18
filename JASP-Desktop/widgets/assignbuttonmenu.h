#ifndef ASSIGNBUTTONMENU_H
#define ASSIGNBUTTONMENU_H

#include <QPushButton>
#include <QMenu>
#include <QAbstractItemView>

#include "widgets/listview.h"

class AssignButtonMenu : public QPushButton
{
	Q_OBJECT
public:
	AssignButtonMenu(QWidget *parent = 0);

	void setSourceAndTarget(ListView *source, ListView *target);

protected:
	ListView *_source;
	QAbstractItemView *_target;

private:
	bool _assignDirection;

private slots:
	void buttonClicked();
	void sourceChanged();
	void targetChanged();
};

#endif // ASSIGNBUTTONMENU_H
