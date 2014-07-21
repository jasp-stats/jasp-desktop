
#include "tablemodelanovadesign.h"

#include <QBrush>
#include <QDebug>

TableModelAnovaDesign::TableModelAnovaDesign(QObject *parent) :
	QAbstractTableModel(parent)
{
	_design.append(24);
	_design.append(3);
}

const QList<int> &TableModelAnovaDesign::design() const
{
	return _design;
}

int TableModelAnovaDesign::rowCount(const QModelIndex &) const
{
	return 1;
}

int TableModelAnovaDesign::columnCount(const QModelIndex &) const
{
	return 2 * _design.size() + 1;
}

QVariant TableModelAnovaDesign::data(const QModelIndex &index, int role) const
{
	if (role == Qt::DisplayRole)
	{
		if (index.column() / 2 == _design.size())
			return "";
		else if ((index.column() % 2) == 0)
			return _design.at(index.column() / 2);
		else
			return "x";
	}
	else if (role == Qt::ForegroundRole)
	{
		if (index.column() == 2 * _design.size() - 1)
			return QBrush(Qt::lightGray);
		else
			return QVariant();
	}
	else if (role == Qt::TextAlignmentRole)
	{
		return Qt::AlignCenter;
	}
	else
	{
		return QVariant();
	}
}

Qt::ItemFlags TableModelAnovaDesign::flags(const QModelIndex &index) const
{
	if ((index.column() % 2) == 0)
		return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsEditable;
	else
		return Qt::ItemIsEnabled;

	return 0;
}

bool TableModelAnovaDesign::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (role != Qt::EditRole)
		return false;

	qDebug() << value.toString();

	bool success;
	int v = value.toInt(&success);

	if ( ! success)
		return false;

	int i = index.column() / 2;
	if (i < _design.size())
	{
		_design[i] = v;

		QVector<int> roles;
		roles.append(Qt::DisplayRole);

		emit dataChanged(index, index, roles);
	}
	else
	{
		beginInsertColumns(QModelIndex(), index.column(), index.column() + 1);

		_design.append(v);

		endInsertColumns();
	}


	return true;
}

