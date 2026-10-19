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

#include "datasetprovider.h"
#include "columnencoder.h"
#include "qutils.h"

#include <memory>

DataSetProvider		*	DataSetProvider::_singleton		= nullptr;

DataSetProvider* DataSetProvider::getProvider(bool inMemory, bool reset, QObject* parent)
{
	if (!_singleton)
		_singleton = new DataSetProvider(inMemory, parent);
	else if (_singleton->_inMemory != inMemory)
	{
		delete _singleton;
		_singleton = new DataSetProvider(inMemory, parent);
	}
	else if (reset)
		_singleton->resetDataSet();

	return _singleton;
}

DataSetProvider::~DataSetProvider()
{
	assert(_singleton == this);
	delete _workspace;
	delete _db;
	_singleton = nullptr;
}

DataSetProvider::DataSetProvider(bool inMemory, QObject *parent) : QAbstractTableModel(parent), _inMemory(inMemory)
{
	_db	= new DatabaseInterface(true, inMemory);
	_workspace = new Workspace();

	new VariableInfo(this);
	_singleton = this;
}

void DataSetProvider::resetDataSet()
{
	if (_workspace)
	{
		_workspace->dbDelete();
		delete _workspace;
	}
	
	_workspace = new Workspace(this);
	_workspace->createDataSet();
}

int	DataSetProvider::rowCount(const QModelIndex &) const
{
	return dataSet()->columnCount();
}

int	DataSetProvider::columnCount(const QModelIndex &) const
{
	return dataSet()->rowCount();
}

QVariant DataSetProvider::data(const QModelIndex & index, int role) const
{
	Column * column = index.row() >= rowCount() ? nullptr : dataSet()->column(index.row());

	if (!column)						return QVariant();
	else if (role == Qt::DisplayRole)	return tq(column->name());
	else								return QVariant(); //QAbstractTableModel::data(index, role);
}

void DataSetProvider::loadDataSet(const std::map<std::string, stringvec > & dataSetStrings, int threshold, bool orderLabelsByValue)
{
	if (!dataSet())
		_workspace->createDataSet();

	dataSet()->beginBatchedToDB();

	int rowCount = 0;
	for (const auto it : dataSetStrings)
		rowCount = rowCount >= it.second.size() ? rowCount : it.second.size();


	dataSet()->setColumnCount(dataSetStrings.size());
	dataSet()->setRowCount(rowCount);

	int colNr = 0;

	for (const auto it : dataSetStrings)
	{
		auto lookup = [&](size_t r)
		{
			return dataSetStrings.at(it.first)[r];
		};

		dataSet()->column(colNr)->initFromLookups(it.first, rowCount, lookup, lookup, it.first, columnType::unknown, {}, threshold, orderLabelsByValue);

		colNr++;
	}

	dataSet()->endBatchedToDB([](float f) {});

	//The desktop must not rely on the process-global ColumnEncoder (that is only meaningful inside the
	//engine's request context); consumers get the dataset's own encoder via provider->columnEncoder().
	dataSet()->encoder().setCurrentNames(dataSet()->getColumnTypesMap());

}

void DataSetProvider::closeDatabase()
{
	_db->close();
}

void DataSetProvider::loadDatabase(const Version & jaspVersion)
{
	delete _workspace;
	_workspace = nullptr;

	try
	{
		_db->close();
		_db->load();
		_db->upgradeDBFromVersion(jaspVersion);

		_workspace = new Workspace(this);
		_workspace->createDataSet();
		dataSet()->dbLoad(1, [](float p) {}, jaspVersion);

		dataSet()->encoder().setCurrentNames(dataSet()->getColumnTypesMap());
	}
	catch (...)
	{
		_workspace = new Workspace(this);
		_workspace->createDataSet();
		throw;
	}
}

QVariantList DataSetProvider::_getDoubleList(Column * column) const
{
	return !column ? QVariantList() : column->getColumnValuesAsDoubleList();
}

QVariantList DataSetProvider::_getStringList(Column * column) const
{

	return !column ? QVariantList() : tvl(tq(column->displaysAsStrings()));
}

QStringList DataSetProvider::_getColumnNames() const
{
	return tq(dataSet()->getColumnNames());
}


QVariant DataSetProvider::provideInfo(varInfoType info, const QString& colName, int row) const
{
	try
	{
		Column * column = dataSet()->column(fq(colName));

		switch(info)
		{
		case varInfoType::VariableType:				return	int(!column ? columnType::unknown : column->type());
		case varInfoType::DoubleValues:				return	_getDoubleList(column);
		case varInfoType::TotalNumericValues:			return	!column ? 0 : column->nonFilteredNumericsCount();
		case varInfoType::TotalLevels:					return	!column ? 0 : (int)column->nonFilteredLevels().size();
		case varInfoType::Labels:						return	!column ? QStringList() : tq(column->nonFilteredLevels());
		case varInfoType::NameRole:					return	Qt::DisplayRole;
		case varInfoType::DataSetRowCount:				return  dataSet()->rowCount();
		case varInfoType::DataSetValue:				return	!column ? "" : tq(column->getValue(row));
		case varInfoType::DataSetValues:				return	_getStringList(column);
		case varInfoType::MaxWidth:					return	100;
		case varInfoType::SignalsBlocked:				return	false;
		case varInfoType::VariableNames:				return	_getColumnNames();
		case varInfoType::DataAvailable:				return	dataSet()->columnCount() > 0;
		case varInfoType::PreviewScale:				return	"";
		case varInfoType::PreviewOrdinal:				return	"";
		case varInfoType::PreviewNominal:				return	"";
		case varInfoType::DataSetPointer:				return	QVariant::fromValue<void*>(dataSet());


		default: break;
		}
	}
	catch(std::exception & e)
	{
		throw e;
	}
	return QVariant("");
}

bool DataSetProvider::absorbInfo(varInfoType info, const QString &colName, int row, QVariant value)
{
	try
	{
		Column * column = dataSet()->column(fq(colName));
		if (!column)
			return false;

		switch(info)
		{
		default:								return false;
		case varInfoType::DataSetValue:		return column->setStringValue(row, fq(value.toString()));
		case varInfoType::DataSetValues:
		{
			int r=0;
			if(dataSet()->rowCount() < value.toList().size())
				dataSet()->setRowCount(value.toList().size(), false);

			for(const QVariant & val : value.toList())
				if (row + r < dataSet()->rowCount())
					column->setStringValue(row + r++, fq(val.toString()), "", false);
			return true;
		}
		}
	}
	catch(std::exception & e)
	{
		throw e;
	}

	return false;
}

