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

#ifndef TABLEMODELANOVADESIGN_H
#define TABLEMODELANOVADESIGN_H

#include "tablemodel.h"
#include "boundmodel.h"
#include "variableinfo.h"

#include "common.h"
#include "terms.h"

#include "options/optionvariables.h"
#include "options/optionstring.h"
#include "options/optionlist.h"
#include "options/optionstable.h"

#include "qutils.h"

typedef QPair<QString, QStringList> Factor;

class TableModelAnovaDesign : public TableModel, public BoundModel, public VariableInfoConsumer
{
	Q_OBJECT
public:
	explicit TableModelAnovaDesign(QObject *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;

	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual int columnCount(const QModelIndex &parent = QModelIndex()) const OVERRIDE;
	virtual QVariant data(const QModelIndex &parent = QModelIndex(), int role = Qt::DisplayRole) const OVERRIDE;
	virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const OVERRIDE;

	virtual Qt::ItemFlags flags(const QModelIndex &index) const OVERRIDE;
	virtual bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) OVERRIDE;
	virtual bool removeRows(int row, int count, const QModelIndex &parent) OVERRIDE;

	QList<Factor> design();

signals:
	void designChanging();
	void designChanged();
	void factorAdded(Terms term);
	void factorRemoved(Terms term);

protected:

	void changeRow(int rowNo, std::string value);
	void deleteRow(int row);

	class Row;

	void refresh();

	std::vector<Options *> _groups;

	OptionsTable *_boundTo;

	QList<Row> _rows;


	class Row
	{
	public:

		Row(QString text, bool isHypothetical, int index, int subIndex = -1)
		{
			this->text = text;
			this->index = index;
			this->subIndex = subIndex;
			this->isHypothetical = isHypothetical;
		}

		bool isHeading() const
		{
			return subIndex == -1;
		}

		QString text;
		int index;
		int subIndex;
		bool isHypothetical;

	};

};

#endif // TABLEMODELANOVADESIGN_H
