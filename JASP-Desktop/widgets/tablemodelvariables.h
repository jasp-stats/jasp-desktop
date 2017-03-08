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

#ifndef TABLEMODELVARIABLES_H
#define TABLEMODELVARIABLES_H

#include <QAbstractListModel>

#include "boundmodel.h"

#include <vector>
#include <string>

#include <QString>
#include <QPair>
#include <QList>
#include <QIcon>
#include <QAbstractItemView>

#include "terms.h"
#include "tablemodel.h"
#include "common.h"
#include "variableinfo.h"

class TableModelVariables : public TableModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	explicit TableModelVariables(QObject *parent = 0);
	
	void setVariableTypesSuggested(int variableTypesSuggested);
	int variableTypesSuggested() const;

	void setVariableTypesAllowed(int variableTypesAllowed);
	int variableTypesAllowed() const;

    virtual int rowCount(const QModelIndex &) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent) const OVERRIDE;
    virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;

    virtual bool insertRows(int row, int count, const QModelIndex &parent) OVERRIDE;
    virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;

    virtual Qt::DropActions supportedDropActions() const OVERRIDE;
    virtual Qt::DropActions supportedDragActions() const OVERRIDE;

    virtual QStringList mimeTypes() const OVERRIDE;
    virtual QMimeData *mimeData(const QModelIndexList &indexes) const OVERRIDE;
    virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) OVERRIDE;
    virtual bool canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const OVERRIDE;

	void setSupportedDropActions(Qt::DropActions actions);
	void setSupportedDragActions(Qt::DropActions actions);

	void setMimeType(const QString &mimeType);

	virtual void mimeDataMoved(const QModelIndexList &indexes) OVERRIDE;

protected:

	Terms _variables;

	bool isAllowed(const Term &term) const;
	bool isSuggested(const Term &term) const;

	bool isDroppingToSelf(const QMimeData *mimeData) const;

	QString _mimeType;

private:

	QAbstractItemView *_defaultTarget;

	Qt::DropActions _dropActions;
	Qt::DropActions _dragActions;

	int _variableTypesSuggested;
	int _variableTypesAllowed;

	QMimeData *_mimeData;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;
	
};

#endif // TABLEMODELVARIABLES_H
