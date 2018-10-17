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

#ifndef BOUNDQMLITEM_H
#define BOUNDQMLITEM_H

#include "options/bound.h"
#include "widgets/qmlitem.h"

class QQuickItem;
class AnalysisQMLForm;

class BoundQMLItem : public virtual QMLItem, public Bound
{

public:
	BoundQMLItem(QQuickItem* item, AnalysisQMLForm* form);
	virtual ~BoundQMLItem();
	
	virtual Option* createOption() = 0;
	virtual Option* boundTo() = 0;
	
};

#endif // BOUNDQMLITEM_H
