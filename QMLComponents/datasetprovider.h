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
#include "workspace.h"
#include "databaseinterface.h"


class ColumnEncoder;
class DataSetProvider : public QAbstractTableModel, public VariableInfoProvider
{
public:
	static DataSetProvider	*	getProvider(bool inMemory, bool reset = true, QObject * parent = nullptr);

	~DataSetProvider();

	DataSet					*	dataSet()	const	{ return _workspace ? _workspace->shownDataSet() : nullptr; }
	void						resetDataSet();

	int							rowCount(	const QModelIndex & parent = QModelIndex())									const	override;
	int							columnCount(const QModelIndex & parent = QModelIndex())									const	override;
	QVariant					data(		const QModelIndex & index, int role = Qt::DisplayRole)						const	override;

	void						loadDataSet(const std::map<std::string, stringvec > & dataSet, int threshold = 10, bool orderLabelsByValue = true);
	void						closeDatabase();
	void						loadDatabase(const Version & jaspVersion);

	QVariant					provideInfo(varInfoType info, const QString& colName = "", int row = 0)		const	override;
	bool						absorbInfo(	varInfoType info, const QString& name, int row, QVariant value)			override;
	QAbstractItemModel		*	providerModel()																					override	{ return this;	}
	ColumnEncoder			*	columnEncoder()																					override	{ DataSet * ds = dataSet(); return ds ? &ds->encoder() : nullptr;	}



private:
	explicit DataSetProvider(bool inMemory = true, QObject* parent = nullptr);

	static DataSetProvider	*	_singleton;

	QVariantList				_getDoubleList(Column * column) const;
	QVariantList				_getStringList(Column * column)	const;
	QStringList					_getColumnNames()				const;

	DatabaseInterface		*	_db					= nullptr;
	Workspace				*	_workspace			= nullptr;
	bool						_inMemory			= true;

};


#endif //DATASETPROVIDER_H
