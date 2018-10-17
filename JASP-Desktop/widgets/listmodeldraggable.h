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
#include "qmllistviewdraggable.h"
#include "analysis/analysisqmldefines.h"

class ListModelDraggable : public ListModel
{
	Q_OBJECT
	
public:
	ListModelDraggable(QMLListView* listView);
	
	bool copyTermsWhenDropped() const						{ return _copyTermsWhenDropped; }
	bool removeTermsWhenDragged() const						{ return _removeTermsWhenDragged; }	
	qmlDropMode dropMode() const							{ return _dropMode; }
	
	void setDropMode(qmlDropMode dropMode)					{ _dropMode = dropMode; }
	
	virtual Terms* termsFromIndexes(const QList<int> &indexes) const;
	virtual bool canAddTerms(Terms* terms) const;
	virtual Terms* addTerms(Terms* terms, int dropItemIndex = -1) ;
	virtual void removeTerms(const QList<int>& indexes);
	virtual void moveTerms(const QList<int>& indexes, int dropItemIndex = -1);

protected:
	bool _removeTermsWhenDragged;
	bool _copyTermsWhenDropped;
	qmlDropMode _dropMode;
		
	bool isAllowed(const Term &term) const;
	bool isSuggested(const Term &term) const;
};

#endif // LISTMODELDRAGGABLE_H
