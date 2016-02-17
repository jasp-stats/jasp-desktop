
#include "variablestablemodel.h"

#include "qutils.h"

VariablesTableModel::VariablesTableModel(QObject *parent)
	: QAbstractTableModel(parent)
{
	_dataSet = NULL;
}

void VariablesTableModel::setDataSet(DataSet *dataSet)
{
	beginResetModel();
	_dataSet = dataSet;
	endResetModel();
}

void VariablesTableModel::clearDataSet()
{
	setDataSet(NULL);
}

QVariant VariablesTableModel::data(const QModelIndex &index, int role) const
{
	if (role != Qt::DisplayRole)
		return QVariant();

	if (_dataSet == NULL)
		return QVariant();

	return tq(_dataSet->columns().at(index.row()).name());
}

int VariablesTableModel::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_dataSet == NULL)
		return 0;

	return _dataSet->columnCount();
}

int VariablesTableModel::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_dataSet == NULL)
		return 0;

	return 1;
}
