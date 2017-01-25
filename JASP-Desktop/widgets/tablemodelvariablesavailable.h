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

#ifndef TABLEMODELVARIABLESAVAILABLE_H
#define TABLEMODELVARIABLESAVAILABLE_H

#include "tablemodelvariables.h"

#include <QList>
#include <QMimeData>

#include "terms.h"
#include "common.h"
#include "variableinfo.h"

class TableModelVariablesAvailable : public TableModelVariables, public VariableInfoProvider
{
	Q_OBJECT
public:
	explicit TableModelVariablesAvailable(QObject *parent = 0);

	void setVariables(const Terms &variables);
    bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;
    virtual QStringList mimeTypes() const OVERRIDE;

	const Terms &allVariables() const;

	void notifyAlreadyAssigned(const Terms &variables);
    bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;
	virtual QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const OVERRIDE;

signals:
	void variablesChanged();

public slots:
	void sendBack(Terms &variables);

private:
	Terms _allVariables;

};

#endif // TABLEMODELVARIABLESAVAILABLE_H
