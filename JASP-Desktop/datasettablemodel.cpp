#include "datasettablemodel.h"

#include <iostream>
#include <fstream>

#include <QSize>
#include <QDebug>

#include "sharedmemory.h"

using namespace std;

DataSetTableModel::DataSetTableModel(QObject *parent) :
    QAbstractTableModel(parent)
{
	_dataSet = new DataSet(SharedMemory::create());
}

DataSetTableModel::~DataSetTableModel()
{
	delete _dataSet;
}

void DataSetTableModel::setDataSet(DataSet* dataSet)
{
    beginResetModel();

	DataSet *old = _dataSet;

	_dataSet = dataSet;

    endResetModel();

	delete old;
}

int DataSetTableModel::rowCount(const QModelIndex &parent) const
{
	return parent.isValid() ? 0 : _dataSet->rowCount();
}

int DataSetTableModel::columnCount(const QModelIndex &parent) const
{
	return parent.isValid() ? 0 : _dataSet->columnCount();
}

QVariant DataSetTableModel::data(const QModelIndex &index, int role) const
{   
    if (role == Qt::DisplayRole)
	{
		string value = _dataSet->columns()[index.column()][index.row()];
		return QVariant(QString::fromUtf8(value.c_str(), value.length()));
	}

    return QVariant();
}

QVariant DataSetTableModel::headerData ( int section, Qt::Orientation orientation, int role) const
{
	if (role == Qt::DisplayRole)
	{
		if (orientation == Qt::Horizontal)
		{
			string value = _dataSet->columns()[section].name();
			return QVariant(QString::fromUtf8(value.c_str(), value.length()));
		}
		else
		{
			return QVariant(section + 1);
		}
	}
	else if (role == Qt::SizeHintRole && orientation == Qt::Vertical)
	{
		return QVariant(QSize(80, -1));
	}
	else if (role == Qt::TextAlignmentRole)
	{
		return QVariant(Qt::AlignCenter);
	}
	else if (role == Qt::UserRole)
	{
		Column &column = _dataSet->columns()[section];
		return QVariant(column.columnType());
	}

	return QVariant();
}

bool DataSetTableModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	bool ok;

	Column &column = _dataSet->columns()[index.column()];
	if (column.dataType() == Column::DataTypeInt)
	{
		int v = value.toInt(&ok);
		if (ok)
			column.setValue(index.row(), v);
		else
			emit badDataEntered(index);

		return ok;
	}

	//_dataSet->columns()[index.column()].setValue(index.row(), v);

	return true;
}

Qt::ItemFlags DataSetTableModel::flags(const QModelIndex &index) const
{
	return Qt::ItemIsSelectable | Qt::ItemIsEditable | Qt::ItemIsEnabled;
}
