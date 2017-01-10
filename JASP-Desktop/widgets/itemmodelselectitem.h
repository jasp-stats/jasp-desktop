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

#ifndef ITEMMODELSELECTITEM_H
#define ITEMMODELSELECTITEM_H

#include <QStandardItemModel>

#include "boundmodel.h"
#include "common.h"
#include "options/optionlist.h"

class ItemModelSelectItem : public QStandardItemModel, public BoundModel
{
public:
	ItemModelSelectItem();

	virtual void bindTo(Option *option) OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	int selectedIndex();
	void setSelected(int index);


private:
	int _selectedIndex;
	OptionList *_boundTo;

};

#endif // ITEMMODELSELECTITEM_H
