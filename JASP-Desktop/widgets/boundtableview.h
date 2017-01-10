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

#ifndef BOUNDTABLEVIEW_H
#define BOUNDTABLEVIEW_H

#include "tableview.h"
#include "bound.h"
#include "boundmodel.h"

class BoundTableView : public TableView, public Bound
{
public:
	BoundTableView(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	virtual void setModel(QAbstractItemModel *model) OVERRIDE;

private:
	BoundModel *_model;
};

#endif // BOUNDTABLEVIEW_H
