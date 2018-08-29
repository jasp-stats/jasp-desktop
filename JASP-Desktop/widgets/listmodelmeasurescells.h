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

#ifndef LISTMODELMEASURESCELLS_H
#define LISTMODELMEASURESCELLS_H

#include "listmodeltermsassignedinterface.h"

class ListModelMeasuresCells : public ListModelTermsAssignedInterface
{
public:
	explicit ListModelMeasuresCells(AnalysisQMLForm *form, QQuickItem* item);

	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;

	virtual void initLevels(const Terms& terms);
	virtual void initVariables(const Terms& terms);
	
	virtual Terms* termsFromIndexes(const QList<int> &indexes) const OVERRIDE;	
	virtual Terms* addTerms(Terms* terms, int dropItemIndex = -1)  OVERRIDE;
	virtual void moveTerms(const QList<int>& indexes, int dropItemIndex = -1) OVERRIDE;		
	virtual void removeTerms(const QList<int>& indexes) OVERRIDE;

	const QList<QString>& variables() const { return _variables; }	

private:
	QList<QString> _levels;
	QList<QString> _variables;
};

#endif // LISTMODELMEASURESCELLS_H
