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
#include "utilities/qutils.h"
#include "columnencoder.h"

DataSetProvider		*	DataSetProvider::_singleton		= nullptr;

DataSetProvider* DataSetProvider::getProvider(bool inMemory, bool reset, QObject* parent)
{
	if (!_singleton)
		_singleton = new DataSetProvider(inMemory, parent);
	else if (reset)
		_singleton->resetDataSet();

	return _singleton;
}

DataSetProvider::~DataSetProvider()
{
	assert(_singleton == this);
	delete VariableInfo::info();
	delete _dataSet;
	delete _db;
	_singleton = nullptr;
}

DataSetProvider::DataSetProvider(bool inMemory, QObject *parent) : QAbstractTableModel(parent)
{
	_db	= new DatabaseInterface(true, inMemory);
	_dataSet = new DataSet();

	new VariableInfo(this);
	_singleton = this;
}

void DataSetProvider::resetDataSet()
{
	if (_dataSet)
	{
		_dataSet->dbDelete();
		delete _dataSet;
	}

	_dataSet = new DataSet();
}

int	DataSetProvider::rowCount(const QModelIndex &) const
{
	return _dataSet->columnCount();
}

int	DataSetProvider::columnCount(const QModelIndex &) const
{
	return _dataSet->rowCount();
}

QVariant DataSetProvider::data(const QModelIndex & index, int role) const
{
	Column * column = index.row() > columnCount() ? nullptr : _dataSet->column(index.row());

	if (!column)						return QVariant();
	else if (role == Qt::DisplayRole)	return tq(column->name());
	else								return QVariant(); //QAbstractTableModel::data(index, role);
}

void DataSetProvider::loadDataSet(const std::map<std::string, stringvec > & dataSet, int threshold, bool orderLabelsByValue)
{

	_dataSet->beginBatchedToDB();

	int rowCount = 0;
	for (const auto it : dataSet)
		rowCount = rowCount >= it.second.size() ? rowCount : it.second.size();


	_dataSet->setColumnCount(dataSet.size());
	_dataSet->setRowCount(rowCount);

	int colNr = 0;

	for (const auto it : dataSet)
	{
		auto lookup = [&](size_t r)
		{
			return dataSet.at(it.first)[r];
		};

		_dataSet->column(colNr)->initFromLookups(it.first, rowCount, lookup, lookup, it.first, columnType::unknown, {}, threshold, orderLabelsByValue);

		colNr++;
	}

	_dataSet->endBatchedToDB([](float f) {});

	ColumnEncoder::columnEncoder()->setCurrentNames(_dataSet->getColumnTypesMap());

}

void DataSetProvider::loadDatabase(const Version & jaspVersion)
{
	delete _dataSet;

	_db->close();
	_db->load();
	_db->upgradeDBFromVersion(jaspVersion);

	_dataSet = new DataSet(0); // Setting 0 for "do nothing" because otherwise we can't pass on jaspVersion
	_dataSet->dbLoad(1, [](float p) {}, jaspVersion);

	ColumnEncoder::columnEncoder()->setCurrentNames(_dataSet->getColumnTypesMap());
}

QVariantList DataSetProvider::_getDoubleList(Column * column) const
{
	QVariantList list;

	if (!column)
		return list;

	for (double value : column->dbls())
		list.append(value);

	return list;

}

QVariantList DataSetProvider::_getStringList(Column * column) const
{
	QVariantList list;

	if (!column)
		return list;

	int rows = _dataSet->rowCount();
	for (int r = 0; r < rows; r++)
		list.append(tq(column->getDisplay(r)));

	return list;
}

QStringList DataSetProvider::_getColumnNames() const
{
	QStringList result;

	int cols = _dataSet->columnCount();
	for (int i = 0; i < cols; i++)
		result.append(tq(_dataSet->column(i)->name()));
	return result;
}


QVariant DataSetProvider::provideInfo(VariableInfo::InfoType info, const QString& colName, int row) const
{
	try
	{
		Column * column = _dataSet->column(fq(colName));

		switch(info)
		{
		case VariableInfo::VariableType:				return	int(!column ? columnType::unknown : column->type());
		case VariableInfo::DoubleValues:				return	_getDoubleList(column);
		case VariableInfo::TotalNumericValues:			return	!column ? 0 : column->nonFilteredNumericsCount();
		case VariableInfo::TotalLevels:					return	!column ? 0 : (int)column->nonFilteredLevels().size();
		case VariableInfo::Labels:						return	!column ? QStringList() : tq(column->nonFilteredLevels());
		case VariableInfo::NameRole:					return	Qt::DisplayRole;
		case VariableInfo::DataSetRowCount:				return  _dataSet->rowCount();
		case VariableInfo::DataSetValue:				return	!column ? "" : tq(column->getValue(row));
		case VariableInfo::DataSetValues:				return	_getStringList(column);
		case VariableInfo::MaxWidth:					return	100;
		case VariableInfo::SignalsBlocked:				return	false;
		case VariableInfo::VariableNames:				return	_getColumnNames();
		case VariableInfo::DataAvailable:				return	_dataSet->columnCount() > 0;
		case VariableInfo::PreviewScale:				return	"";
		case VariableInfo::PreviewOrdinal:				return	"";
		case VariableInfo::PreviewNominal:				return	"";
		case VariableInfo::DataSetPointer:				return	QVariant::fromValue<void*>(_dataSet);


		default: break;
		}
	}
	catch(std::exception & e)
	{
		throw e;
	}
	return QVariant("");
}

bool DataSetProvider::absorbInfo(VariableInfo::InfoType info, const QString &colName, int row, QVariant value)
{
	try
	{
		Column * column = _dataSet->column(fq(colName));
		if (!column)
			return false;

		switch(info)
		{
		default:								return false;
		case VariableInfo::DataSetValue:		return column->setStringValue(row, fq(value.toString()));
		case VariableInfo::DataSetValues:
		{
			int r=0;
			if(_dataSet->rowCount() < value.toList().size())
				_dataSet->setRowCount(value.toList().size(), false);

			for(const QVariant & val : value.toList())
				if (row + r < _dataSet->rowCount())
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

