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

#ifndef TABLEMODELVARIABLESASSIGNED_H
#define TABLEMODELVARIABLESASSIGNED_H

#include "tablemodelvariables.h"

#include "tablemodelvariablesavailable.h"
#include "options/optionterms.h"

#include "column.h"

class TableModelVariablesAssigned : public TableModelVariables, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelVariablesAssigned(QObject *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	void setSource(TableModelVariablesAvailable *source);

	virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
	virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
	virtual void mimeDataMoved(const QModelIndexList &indices) OVERRIDE;

	void setSorted(bool sorted);
	const Terms &assigned() const;

signals:
	void assignmentsChanging();
	void assignmentsChanged();

	void assignedTo(const Terms &variables);
	void unassigned(const Terms &variables);

private slots:
	void sourceVariablesChanged();
	void sendBack();
	void delayAssignDroppedData();

private:
	void assign(const Terms &variables);
	void unassign(const Terms &variables);
	void setAssigned(const Terms &variables);

	OptionTerms *_boundTo;
	TableModelVariablesAvailable *_source;
	bool _sorted;

	Terms _toSendBack;
	Terms _delayDropped;
};

#endif // TABLEMODELVARIABLESASSIGNED_H
