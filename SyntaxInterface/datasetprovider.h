//
// Copyright (C) 2013-2025 University of Amsterdam
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

#ifndef DATASETPROVIDER_H
#define DATASETPROVIDER_H

#include <QAbstractTableModel>
#include "variableinfo.h"
#include "dataset.h"
#include "databaseinterface.h"


class DataSetProvider : public QAbstractTableModel, public VariableInfoProvider
{

public:
	static DataSetProvider	*	getProvider(bool inMemory, bool reset = true);

	DataSet					*	dataSet()	{ return _dataset; }
	void						resetDataSet();

	int							rowCount(	const QModelIndex & parent = QModelIndex())									const	override;
	int							columnCount(const QModelIndex & parent = QModelIndex())									const	override;
	QVariant					data(		const QModelIndex & index, int role = Qt::DisplayRole)						const	override;


	QVariant					provideInfo(VariableInfo::InfoType info, const QString& colName = "", int row = 0)		const	override;
	bool						absorbInfo(	VariableInfo::InfoType info, const QString& name, int row, QVariant value)			override;
	QAbstractItemModel		*	providerModel()																					override	{ return this;	}

private:
	explicit DataSetProvider(bool inMemory = true, QObject* parent = nullptr);

	static DataSetProvider	*	_singleton;

	QVariantList				_getDoubleList(Column * column) const;
	QVariantList				_getStringList(Column * column)	const;
	QStringList					_getColumnNames()				const;

	DatabaseInterface		*	_db					= nullptr;
	DataSet					*	_dataset			= nullptr;

};


#endif //DATASETPROVIDER_H
