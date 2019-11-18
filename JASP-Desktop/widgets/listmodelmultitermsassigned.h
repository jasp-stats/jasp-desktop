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

#ifndef LISTMODELMULTITERMSASSIGNED_H
#define LISTMODELMULTITERMSASSIGNED_H

#include "listmodelassignedinterface.h"

class ListModelMultiTermsAssigned: public ListModelAssignedInterface
{
	Q_OBJECT
public:
	ListModelMultiTermsAssigned(QMLListView* listView, int columns = 2);
	
	int rowCount(const QModelIndex &parent = QModelIndex())							const override;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole)				const override;

	Terms* termsFromIndexes(const QList<int> &indexes)								const override;
	bool canAddTerms(Terms *terms) const override;
	Terms* addTerms(Terms *terms, int dropItemIndex = -1, const QString& assignOption = "") override;
	void moveTerms(const QList<int>& indexes, int dropItemIndex = -1)						override;	
	void removeTerms(const QList<int> &indexes)												override;

protected:
	int	_columns = 2;
};

#endif // LISTMODELMULTITERMSASSIGNED_H
