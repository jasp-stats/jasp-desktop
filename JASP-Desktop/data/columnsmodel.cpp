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
		return QString("The '") + QString::fromStdString(col.name()) + "'-column " + (col.columnType() == Column::ColumnType::ColumnTypeScale ? "which can be used in numerical comparisons" : "which can only be used in (in)equivalence comparisons");

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
