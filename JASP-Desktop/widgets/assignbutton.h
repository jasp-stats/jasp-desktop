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

	void setSourceAndTarget(ListView *source, ListView *target);
	void setSourceAndTarget(ListView *source, BoundPairsTable *target);

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

#endif // ASSIGNBUTTON_H
