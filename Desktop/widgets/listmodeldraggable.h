//
// Copyright (C) 2013-2018 University of Amsterdam
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

#ifndef LISTMODELDRAGGABLE_H
#define LISTMODELDRAGGABLE_H

#include "listmodel.h"
#include "analysis/jaspcontrol.h"

class RowControls;

class ListModelDraggable : public ListModel
{
	Q_OBJECT
	
public:
	ListModelDraggable(JASPListControl* listView);
	~ListModelDraggable();

	bool copyTermsWhenDropped() const						{ return _copyTermsWhenDropped; }
	JASPControl::DropMode dropMode() const				{ return _dropMode; }
	
	void setDropMode(JASPControl::DropMode dropMode)	{ _dropMode = dropMode; }
	void setCopyTermsWhenDropped(bool copy)					{ _copyTermsWhenDropped = copy; }
	
	virtual Terms termsFromIndexes(const QList<int> &indexes)					const;
	virtual Terms canAddTerms(const Terms& terms)								const;
	virtual Terms addTerms(const Terms& terms, int dropItemIndex = -1, const RowControlsValues& rowValues = RowControlsValues());
	virtual void removeTerms(const QList<int>& indexes);
	virtual void moveTerms(const QList<int>& indexes, int dropItemIndex = -1);

signals:
	void destroyed(ListModelDraggable * me);

protected:
	bool						_copyTermsWhenDropped;
	bool						_addNewAvailableTermsToAssignedModel	= false;
	bool						_allowAnalysisOwnComputedColumns		= true;
	JASPControl::DropMode		_dropMode								= JASPControl::DropMode::DropNone;
		
	bool						isAllowed(const Term &term) const;
};

#endif // LISTMODELDRAGGABLE_H
