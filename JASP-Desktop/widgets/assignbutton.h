#ifndef ASSIGNBUTTON_H
#define ASSIGNBUTTON_H

#include <QPushButton>

#include "listmodelvariablesavailable.h"
#include "listmodelvariablesassigned.h"
#include "listview.h"
#include "boundpairstable.h"

class BoundPairsTable;

class AssignButton : public QPushButton
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
