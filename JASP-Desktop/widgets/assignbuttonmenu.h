#ifndef ASSIGNBUTTONMENU_H
#define ASSIGNBUTTONMENU_H

#include "button.h"
#include "droptarget.h"

class AssignButtonMenu : public Button
{
	Q_OBJECT
public:
	AssignButtonMenu(QWidget *parent = 0);

	void setSourceAndTarget(DropTarget *source, DropTarget *target);

protected:
	DropTarget *_source;
	DropTarget *_target;

private:
	bool _assignDirection;

private slots:
	void buttonClicked();
	void sourceChanged();
	void targetChanged();
};

#endif // ASSIGNBUTTONMENU_H
