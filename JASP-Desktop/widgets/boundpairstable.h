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

#ifndef BOUNDPAIRSTABLE_H
#define BOUNDPAIRSTABLE_H

#include <QTableView>
#include "bound.h"
#include "availablefieldslistview.h"
#include "assignbutton.h"
#include "tablemodelpairsassigned.h"
#include "tableview.h"

class BoundPairsTable : public TableView, public Bound
{
	Q_OBJECT
public:
	explicit BoundPairsTable(QWidget *parent = 0);

	virtual void setModel(QAbstractItemModel *model) OVERRIDE;
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void notifyDragWasDropped() OVERRIDE;

protected:

	void resizeEvent(QResizeEvent *e) OVERRIDE;
	void moveEvent(QMoveEvent *e) OVERRIDE;

	void setupKey();
	void repositionKey();

private:

	TableModelPairsAssigned *_tableModel;
	QWidget *_variableTypeKey;

};

#endif // BOUNDPAIRSTABLE_H
