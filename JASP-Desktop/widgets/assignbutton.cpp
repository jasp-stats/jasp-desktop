#include "assignbutton.h"

AssignButton::AssignButton(QWidget *parent) :
	QPushButton(parent)
{
	setAssignDirection(true);
}

void AssignButton::setAssignDirection(bool assign)
{
	_assignDirection = assign;

	if (assign)
		setIcon(QIcon(":/images/arrow-right.png"));
	else
		setIcon(QIcon(":/images/arrow-left.png"));
}

bool AssignButton::isAssign()
{
	return _assignDirection;
}


