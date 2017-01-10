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

#include "tablemodelpairsassigned.h"
#include "column.h"

#include <QMimeData>
#include <QTimer>
#include <QDebug>

using namespace std;

TableModelPairsAssigned::TableModelPairsAssigned(QObject *parent)
	: TableModel(parent)
{
	_boundTo = NULL;
	_source = NULL;

	_variableTypesAllowed = 0;
	_variableTypesSuggested = Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale;
}

void TableModelPairsAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariablesGroups *>(option);

	if (_boundTo == NULL)
	{
		qDebug() << "TableModelPairsAssigned::bindTo(); Could not bind to option";
		return;
	}

	if (_source == NULL)
	{
		qDebug() << "TableModelPairsAssigned::bindTo(); source not set";
		return;
	}

	beginResetModel();

	Terms terms = _boundTo->value();
	_values = terms.asQListOfQLists();

	endResetModel();
}

void TableModelPairsAssigned::setSource(TableModelVariablesAvailable *source)
{
	_source = source;
	this->setInfoProvider(source);
}

int TableModelPairsAssigned::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	return _values.size();
}

int TableModelPairsAssigned::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	return 2;
}

QVariant TableModelPairsAssigned::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		const QStringList &row = _values.at(index.row());
		return row.at(index.column());
	}

	return QVariant();
}

Qt::ItemFlags TableModelPairsAssigned::flags(const QModelIndex &index) const
{	
	if (index.isValid())
		return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsDragEnabled | Qt::ItemIsDropEnabled;
	else
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
}

Qt::DropActions TableModelPairsAssigned::supportedDropActions() const
{
	return Qt::CopyAction;
}

Qt::DropActions TableModelPairsAssigned::supportedDragActions() const
{
	return Qt::MoveAction;
}

QStringList TableModelPairsAssigned::mimeTypes() const
{
	QStringList types;

	types << "application/vnd.list.variable";

	return types;
}

QMimeData *TableModelPairsAssigned::mimeData(const QModelIndexList &indexes) const
{
	Q_UNUSED(indexes);

	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << 0;

	mimeData->setData("application/vnd.list.variable", encodedData);

	return mimeData;
}

bool TableModelPairsAssigned::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	if (action == Qt::IgnoreAction)
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	if (mimeTypes().contains("application/vnd.list.variable"))
	{
		QByteArray encodedData = data->data("application/vnd.list.variable");
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		Terms terms;
		terms.set(encodedData);

		if (parent.isValid()) // drop into cell
		{
			QStringList row = _values.at(parent.row());
			row.replace(parent.column(), terms.at(0).asQString());
			_values.replace(parent.row(), row);
			emit dataChanged(parent, parent);
		}
		else if (row == -1 && _values.size() > 0 && _values.last().last() == "")
		{
			int row = _values.length() - 1;
			int column = _values.at(row).length() - 1;

			_values.last().last() = terms.at(0).asQString();

			emit dataChanged(index(row, column), index(row, column));
		}
		else
		{
			int beginRow;

			if (row != -1)
				beginRow = row;
			else
				beginRow = rowCount(QModelIndex());

			beginInsertRows(QModelIndex(), beginRow, beginRow);

			if (terms.size() == 1)
			{
				QList<QString> newRow;
				newRow.append(terms.at(0).asQString());
				newRow.append("");

				_values.insert(beginRow, newRow);
			}
			else
			{
				QList<QString> newRow;
				newRow.append(terms.at(0).asQString());
				newRow.append(terms.at(1).asQString());

				_values.insert(beginRow, newRow);
			}

			endInsertRows();
		}

		assignToOption();

		return true;
	}

	return false;
}

bool TableModelPairsAssigned::canDropMimeData(const QMimeData *data, Qt::DropAction, int row, int column, const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	Q_UNUSED(row);
	Q_UNUSED(column);

	if (mimeTypes().contains("application/vnd.list.variable"))
	{
		QByteArray encodedData = data->data("application/vnd.list.variable");

		Terms variables;
		variables.set(encodedData);

		foreach (const Term &variable, variables)
		{
			if ( ! isAllowed(variable))
				return false;
		}

		return true;
	}
	else
	{
		return false;
	}
}

bool TableModelPairsAssigned::isAllowed(const Term &term) const
{
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	int variableType = v.toInt();

	return variableType == 0 || variableType & _variableTypesAllowed;
}

bool TableModelPairsAssigned::insertRows(int row, int count, const QModelIndex &parent)
{
	beginInsertRows(parent, row, row + count - 1);
	for (int i = 0; i < count; i++)
	{
		QList<QString> newRow;
		newRow.append("");
		newRow.append("");
		_values.insert(row, newRow);
	}
	endInsertRows();

	return true;
}

void TableModelPairsAssigned::mimeDataMoved(const QModelIndexList &indexes)
{
	beginResetModel();

	QModelIndexList sorted = indexes;

	int lastRowDeleted = -1;

	qSort(sorted.begin(), sorted.end(), qGreater<QModelIndex>());

	foreach (const QModelIndex &index, sorted)
	{
		int row = index.row();
		if (row != lastRowDeleted)
			_values.removeAt(row);
		lastRowDeleted = row;
	}

	endResetModel();

	assignToOption();
}

void TableModelPairsAssigned::assignToOption()
{
	if (_boundTo != NULL)
	{
		vector<vector<string> > pairs;

		foreach (const QStringList &qPair, _values)
		{
			vector<string> pair;
			pair.push_back(qPair.first().toStdString());
			pair.push_back(qPair.at(1).toStdString());
			pairs.push_back(pair);
		}

		_boundTo->setValue(pairs);
	}
}

void TableModelPairsAssigned::setVariableTypesSuggested(int variableTypesSuggested)
{
	_variableTypesSuggested = variableTypesSuggested;
}

int TableModelPairsAssigned::variableTypesSuggested() const
{
	return _variableTypesSuggested;
}

void TableModelPairsAssigned::setVariableTypesAllowed(int variableTypesAllowed)
{
	_variableTypesAllowed = variableTypesAllowed;
}

int TableModelPairsAssigned::variableTypesAllowed() const
{
	return _variableTypesAllowed;
}
