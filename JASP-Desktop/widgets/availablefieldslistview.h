#ifndef AVAILABLEFIELDSLISTVIEW_H
#define AVAILABLEFIELDSLISTVIEW_H

#include <vector>

#include <QListView>

#include "assignbutton.h"
#include "listview.h"

class AvailableFieldsListView : public ListView
{
	Q_OBJECT
public:
	explicit AvailableFieldsListView(QWidget *parent = 0);

};

#endif // AVAILABLEFIELDSLISTVIEW_H
