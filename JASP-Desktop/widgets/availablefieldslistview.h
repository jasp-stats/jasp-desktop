#ifndef AVAILABLEFIELDSLISTVIEW_H
#define AVAILABLEFIELDSLISTVIEW_H

#include <vector>

#include <QListView>

#include "assignbutton.h"

class AvailableFieldsListView : public QListView
{
	Q_OBJECT
public:
	explicit AvailableFieldsListView(QWidget *parent = 0);

	void addAssignButton(AssignButton *button);

	QStringList selectedFields() const;

protected:
	virtual void focusInEvent(QFocusEvent *event) override;
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected) override;
	
signals:
	
public slots:

private:
	std::vector<AssignButton *> _assignButtons;

};

#endif // AVAILABLEFIELDSLISTVIEW_H
