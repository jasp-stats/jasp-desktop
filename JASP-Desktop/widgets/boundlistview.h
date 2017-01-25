//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef BOUNDLISTVIEW_H
#define BOUNDLISTVIEW_H


#include "bound.h"

#include <QStringListModel>
#include <QListView>
#include <QIcon>

#include "options/optionvariables.h"

#include "availablefieldslistview.h"
#include "dataset.h"
#include "tablemodelvariablesassigned.h"
#include "listview.h"

class BoundListView : public ListView, public Bound
{
	Q_OBJECT

public:
	BoundListView(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;

	void setAssignButton(AssignButton *button);
	void setAvailableFieldsListView(AvailableFieldsListView *listView);

	virtual void setModel(QAbstractItemModel *model) OVERRIDE;

protected:
	virtual void resizeEvent(QResizeEvent *e) OVERRIDE;
	virtual void moveEvent(QMoveEvent *e) OVERRIDE;

	TableModelVariablesAssigned *_variablesListModel;

private:

	QWidget *_variableTypeKey;

private slots:
	void repositionKey();

};

#endif // BOUNDLISTVIEW_H
