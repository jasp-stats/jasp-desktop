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

#ifndef TABLEMODELVARIABLESOPTIONS_H
#define TABLEMODELVARIABLESOPTIONS_H

#include <QAbstractTableModel>

#include "boundmodel.h"
#include "options/optionstable.h"
#include "terms.h"

class TableModelVariablesOptions : public QAbstractTableModel, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelVariablesOptions(QObject *parent = 0);

	void bindTo(Option *option) OVERRIDE;
	void unbind() OVERRIDE;

	int rowCount(const QModelIndex &) const OVERRIDE;
	int columnCount(const QModelIndex &parent) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role) const OVERRIDE;
	bool setData(const QModelIndex &index, const QVariant &value, int role) OVERRIDE;
	Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	void setVariables(const Terms &variables);
	const Terms& variables() const;

private:

	Terms _variables;
	std::vector<Options*> _rows;
	OptionsTable *_boundTo;

};

#endif // TABLEMODELVARIABLESOPTIONS_H
