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

#ifndef LISTMODELVARIABLESTABLE_H
#define LISTMODELVARIABLESTABLE_H

#include "listmodel.h"
#include "analysis/options/terms.h"
#include "analysis/boundqmlitem.h"

class ListModelVariablesTable : public ListModel
{
	Q_OBJECT
public:
	
	ListModelVariablesTable(AnalysisQMLForm *form, QQuickItem *item);
	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;	
	
	virtual void initTerms(const Terms& terms);
	
	const QMap<QString, QList<BoundQMLItem*> >& rows() const;
	
protected:
	
	Terms _terms;
	QMap<QString, QList<BoundQMLItem*> > _rows;
	QMap<QString, QList<BoundQMLItem*> > _cachedRows;
	
private slots:
	void removeRowSlot(QString term);
	void addRowSlot(QString term, QVariant controls);
};

#endif // LISTMODELVARIABLESTABLE_H
