//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//


#include "columnsmodel.h"


QVariant ColumnsModel::data(const QModelIndex &index, int role) const
{
	if(index.row() >= rowCount()) return QVariant();

	Column & col=_dataSet->column(index.row());

	if(role == NameRole)
		return QString::fromStdString(col.name());
	else if(role ==TypeRole)
		return "column";
	else if(role == IconSourceRole)
		switch(col.columnType())
		{
		case Column::ColumnType::ColumnTypeScale:		return "qrc:/icons/variable-scale.svg";
		case Column::ColumnType::ColumnTypeOrdinal:		return "qrc:/icons/variable-ordinal.svg";
		case Column::ColumnType::ColumnTypeNominal:		return "qrc:/icons/variable-nominal.svg";
		case Column::ColumnType::ColumnTypeNominalText:	return "qrc:/icons/variable-nominal-text.svg";
		default:										return "";
		}
	else if(role == ToolTipRole)
	{
		QString usedIn = col.columnType() == Column::ColumnType::ColumnTypeScale ? "which can be used in numerical comparisons" : col.columnType() == Column::ColumnType::ColumnTypeOrdinal ? "which can only be used in (in)equivalence, greater and lesser than comparisons" : "which can only be used in (in)equivalence comparisons";
		return QString("The '") + QString::fromStdString(col.name()) + "'-column " + usedIn;
	}

	return QVariant();
}

QHash<int, QByteArray> ColumnsModel::roleNames() const {
	static const auto roles = QHash<int, QByteArray>{
		{ NameRole,					"columnName"},
		{ TypeRole,					"type"},
		{ IconSourceRole,			"columnIcon"},
		{ ToolTipRole,				"toolTip"}
	};

	return roles;
}

void ColumnsModel::setDataSet(DataSet *dataSet)
{
	beginResetModel();
	_dataSet = dataSet;
	endResetModel();
}

void ColumnsModel::refreshColumn(Column * column)
{
	int rowChanged = _dataSet->getColumnIndex(column->name());
	emit dataChanged(index(rowChanged, 0), index(rowChanged, columnCount()));
}

void ColumnsModel::datasetHeaderDataChanged(Qt::Orientation orientation, int first, int last)
{
	emit dataChanged(index(first, 0), index(last, columnCount()-1));
}
