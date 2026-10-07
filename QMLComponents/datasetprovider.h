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

	///< With a title: named load for the multi-dataset (syntax-mode) flow - replaces the contents
	///< of the dataset with that title or adds a new one; every titled dataset gets a real id and
	///< encoder prefix. Without: legacy behaviour, fill the shown dataset.
	void						loadDataSet(const std::map<std::string, stringvec > & dataSet, int threshold = 10, bool orderLabelsByValue = true, const QString & title = QString());
	///< id of the first dataset loaded since the last resetDataSet() (the primary of a syntax-mode
	///< multi-dataset run, matching the wrapper's datasets[[1]]); -1 when nothing was loaded yet.
	int							firstLoadedDataSetId() const { return _firstLoadedDataSetId; }
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
	int							_firstLoadedDataSetId = -1;

};


#endif //DATASETPROVIDER_H
