
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

	_nominalTextAllowed = true;
	_variableTypesSuggested = Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale;
}

void TableModelPairsAssigned::setIsNominalTextAllowed(bool allowed)
{
	_nominalTextAllowed = allowed;
}

bool TableModelPairsAssigned::nominalTextAllowed()
{
	return _nominalTextAllowed;
}

void TableModelPairsAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariablesGroups *>(option);

	if (_boundTo == NULL)
	{
		qDebug() << "TableModelVariablesAssigned::bindTo(); Could not bind to option";
		return;
	}

	if (_source == NULL)
	{
		qDebug() << "TableModelVariablesAssigned::bindTo(); source not set";
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

		int count;

		stream >> count;

		if (count == 0)
			return false;

		QString item1;
		stream >> item1;

		if (parent.isValid()) // drop into cell
		{
			QStringList row = _values.at(parent.row());
			row.replace(parent.column(), item1);
			_values.replace(parent.row(), row);
			emit dataChanged(parent, parent);
		}
		else if (row == -1 && _values.size() > 0 && _values.last().last() == "")
		{
			int row = _values.length() - 1;
			int column = _values.at(row).length() - 1;

			_values.last().last() = item1;

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

			if (count == 1)
			{
				QList<QString> newRow;
				newRow.append(item1);
				newRow.append("");

				_values.insert(beginRow, newRow);
			}
			else
			{
				QString item2;
				stream >> item2;

				QList<QString> newRow;
				newRow.append(item1);
				newRow.append(item2);

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
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		if (stream.atEnd()) // is empty
			return false;

		int count;

		stream >> count;

		return count != 0;
	}

	return false;
}

bool TableModelPairsAssigned::isForbidden(int variableType) const
{
	return _nominalTextAllowed == false && variableType == Column::ColumnTypeNominalText;
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
		_boundTo->setValue(Terms(_values).asVectorOfVectors());
}

void TableModelPairsAssigned::setVariableTypesSuggested(int variableTypesSuggested)
{
	_variableTypesSuggested = variableTypesSuggested;
}

int TableModelPairsAssigned::variableTypesSuggested()
{
	return _variableTypesSuggested;
}


