//
// Copyright (C) 2013-2021 University of Amsterdam
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

#ifndef BOUNDCONTROLTABLEVIEW_H
#define BOUNDCONTROLTABLEVIEW_H

#include "boundcontrolbase.h"
#include "models/listmodeltableviewbase.h"

class TableViewBase;

class BoundControlTableView : public BoundControlBase
{
public:
	BoundControlTableView(TableViewBase* tableView);

	bool					isJsonValid(const Json::Value& value)		override;
	Json::Value				createJson()								override;
	void					bindTo(const Json::Value& value)			override;
	void					resetBoundValue()							override;

protected:
	virtual void			fillTableTerms(const Json::Value& value, ListModelTableViewBase::TableTerms& tableTerms);
	virtual void			fillBoundValue(Json::Value& value, const ListModelTableViewBase::TableTerms& tableTerms);
	Json::Value				_defaultValue(int colIndex = -1, int rowIndex = -1);

	TableViewBase			* _tableView	= nullptr;
};

#endif // BOUNDCONTROLTABLEVIEW_H
