#ifndef ASSIGNBUTTON_H
#define ASSIGNBUTTON_H

#include <QPushButton>

class AssignButton : public QPushButton
{
	Q_OBJECT
public:
	explicit AssignButton(QWidget *parent = 0);

	void setAssignDirection(bool assign);
	bool isAssign();

private:
	bool _assignDirection;

};

#endif // ASSIGNBUTTON_H
