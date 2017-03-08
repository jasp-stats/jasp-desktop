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

#ifndef TABLEMODELCONTRASTS_H
#define TABLEMODELCONTRASTS_H

#include "tablemodel.h"
#include "boundmodel.h"
#include "options/options.h"
#include "options/optionstable.h"
#include "terms.h"

class TableModelContrasts : public TableModel, public BoundModel
{
	Q_OBJECT
public:
	explicit TableModelContrasts(QObject *parent = 0);

	void bindTo(Option *option) OVERRIDE;
	void setLabels(const Terms &levels);

	int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const OVERRIDE;
	bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::DisplayRole) OVERRIDE;
	Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

signals:

public slots:

private:
	Terms _labels;
	std::vector<Options *> _contrasts;
	OptionsTable *_boundTo;

};

#endif // TABLEMODELCONTRASTS_H
