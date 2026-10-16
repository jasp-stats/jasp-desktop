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

	bool					isJsonValid(const Json::Value& value)	const	override;
	//the no-arg createJson override here would otherwise HIDE the JsonFormat overload
	//declared on BoundControl/Base (C++ name lookup), which the composite controls forward
	//through this concrete pointer (mds-plan D3.1):
	using BoundControlBase::createJson;
	Json::Value				createJson()							const	override;
	void					bindTo(const Json::Value& value)				override;
	void					resetBoundValue()								override;

protected:
	///< mds-plan D3.1: the mirror value of a tableview is built from the live table model through
	/// fillBoundValue (that is what resetBoundValue/the model-change path writes), not from
	/// createJson's default builder - the generator reads the same live TableTerms through the same
	/// virtual fill, so both shapes stay one source of truth. (Contrasts/FilteredDataEntry/Grid
	/// inherit this and only their fillBoundValue override runs.)
	Json::Value				createEngineValue()						const	override;
	virtual void			fillTableTerms(const Json::Value& value, ListModelTableViewBase::TableTerms& tableTerms);
	virtual void			fillBoundValue(Json::Value& value, const ListModelTableViewBase::TableTerms& tableTerms)	const;
	Json::Value				_defaultValue(int colIndex = -1, int rowIndex = -1) const;

	TableViewBase			* _tableView	= nullptr;
};

#endif // BOUNDCONTROLTABLEVIEW_H
