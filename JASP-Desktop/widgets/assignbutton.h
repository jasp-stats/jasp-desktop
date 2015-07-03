#ifndef ASSIGNBUTTON_H
#define ASSIGNBUTTON_H

#include "listview.h"
#include "button.h"

class AssignButton : public Button
{
	Q_OBJECT
public:
	explicit AssignButton(QWidget *parent = 0);

	void setAssignDirection(bool assign);
	bool isAssign();

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

#endif // ASSIGNBUTTON_H
