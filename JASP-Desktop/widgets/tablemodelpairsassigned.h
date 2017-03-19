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

#ifndef TABLEMODELPAIRSASSIGNED_H
#define TABLEMODELPAIRSASSIGNED_H

#include <QAbstractListModel>

#include "options/optionvariablesgroups.h"
#include "tablemodelvariables.h"
#include "tablemodel.h"
#include "droptarget.h"
#include "tablemodelvariablesavailable.h"

class TableModelPairsAssigned : public TableModel, public BoundModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	explicit TableModelPairsAssigned(QObject *parent = 0);

	void setVariableTypesSuggested(int variableTypesSuggested);
	int variableTypesSuggested() const;

	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed() const;

	void bindTo(Option *option) OVERRIDE;
	int rowCount(const QModelIndex &parent) const OVERRIDE;
	int columnCount(const QModelIndex &parent) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

	virtual Qt::DropActions supportedDropActions() const OVERRIDE;
	virtual Qt::DropActions supportedDragActions() const OVERRIDE;
	virtual QStringList mimeTypes() const OVERRIDE;
	virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
	virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

	void setSource(TableModelVariablesAvailable *source);

protected:

	bool isAllowed(const Term &term) const;
	void assignToOption();

private:
	int _variableTypesAllowed;
	int _variableTypesSuggested;

	TableModelVariablesAvailable *_source;

	OptionVariablesGroups *_boundTo;
	QList<QList<QString> > _values;

	void pairsChanged();
	
};

#endif // TABLEMODELPAIRSASSIGNED_H
