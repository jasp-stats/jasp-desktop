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

#ifndef ITEMMODELSELECTVARIABLE_H
#define ITEMMODELSELECTVARIABLE_H

#include <QAbstractListModel>
#include "boundmodel.h"
#include "common.h"
#include "options/optionvariable.h"
#include "widgets/tablemodelvariablesavailable.h"
#include "variableinfo.h"

typedef QPair<QString, int> ColumnInfo;

class ItemModelSelectVariable : public QAbstractListModel, public BoundModel
{
	Q_OBJECT
public:
	explicit ItemModelSelectVariable(QObject *parent = 0);

	virtual int rowCount(const QModelIndex &parent) const OVERRIDE;
	virtual QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;

	void bindTo(Option* option) OVERRIDE;

	void setSource(TableModelVariablesAvailable *source);

private slots:
	void variablesChangedHandler();

private:

	void updateSelected();

	int _selectedIndex;
	TableModelVariablesAvailable *_source;
	OptionVariable *_boundTo;

	QIcon _nominalTextIcon;
	QIcon _nominalIcon;
	QIcon _ordinalIcon;
	QIcon _scaleIcon;
};

#endif // ITEMMODELSELECTVARIABLE_H
