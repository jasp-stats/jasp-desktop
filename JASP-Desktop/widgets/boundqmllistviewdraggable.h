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

#ifndef BOUNDQMLLISTVIEWDRAGGABLE_H
#define BOUNDQMLLISTVIEWDRAGGABLE_H

#include "analysis/boundqmlitem.h"
#include "qmllistviewdraggable.h"
#include "analysis/options/terms.h"

class ListModelAvailableInterface;
class ListModelAssignedInterface;

class BoundQMLListViewDraggable : public QMLListViewDraggable, public BoundQMLItem
{
	Q_OBJECT
	
	typedef QPair<int, QMap<QString, BoundQMLItem*> > RowBoundItemType;
	typedef QPair<int, QMap<QString, QQuickItem*> > RowQuickItemType;
	
public:
	BoundQMLListViewDraggable(QQuickItem* item, AnalysisForm* form);
	
	virtual void setUp() OVERRIDE;
	
protected:
	ListModelAvailableInterface* _sourceModel;
	ListModelAssignedInterface* assignedModel();
	
	bool _hasExtraControlColumns;
	QMap<QString, QMap<QString, QString> > _extraControlColumns;
	QMap<QString, QMap<QString, BoundQMLItem*> > _rowsWithControls;
	
	void addExtraOptions(Options* options);
	
protected slots:
	void removeRowWithControlsHandler(int index, QString name);
	void addRowWithControlsHandler(int index, QString name, QVariant controls);

private:
	Terms _tempTerms;
	QMap<QString, RowBoundItemType> _cachedRows;
	QMap<QString, RowQuickItemType> _addedRows;
	
	void _mapRowsWithBoundItems();
	void _resetQuickItems(const QMap<QString, QQuickItem*>& quickItems, const QMap<QString, BoundQMLItem*>& boundItems);
};

#endif // BOUNDQMLLISTVIEWDRAGGABLE_H
