#include "listmodelvariables.h"

#include <QMimeData>

#include "column.h"

#include <QDebug>

ListModelVariables::ListModelVariables(QObject *parent) :
	QAbstractListModel(parent)
{
	_variableTypesAllowed = Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale;

	_nominalIcon = QIcon(":/icons/variable-nominal.png");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.png");
	_scaleIcon = QIcon(":/icons/variable-scale.png");

	_dropActions = Qt::MoveAction;
	_dragActions = Qt::MoveAction;

	_defaultTarget = NULL;

	_mimeType = "application/vnd.list.variable";
}

void ListModelVariables::setVariableTypesAllowed(int variableTypesAllowed)
{
	_variableTypesAllowed = variableTypesAllowed;
}

int ListModelVariables::variableTypesAllowed()
{
	return _variableTypesAllowed;
}

int ListModelVariables::rowCount(const QModelIndex &) const
{
	return _variables.length();
}

QVariant ListModelVariables::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole)
	{
		return QVariant(_variables.at(row).first);
	}
	else if (role == Qt::DecorationRole)
	{
		switch (_variables.at(row).second)
		{
		case Column::ColumnTypeNominal:
			return QVariant(_nominalIcon);
		case Column::ColumnTypeOrdinal:
			return QVariant(_ordinalIcon);
		case Column::ColumnTypeScale:
			return QVariant(_scaleIcon);
		default:
			return QVariant();
		}
	}
	else
	{
		return QVariant();
	}
}

bool ListModelVariables::removeRows(int row, int count, const QModelIndex &parent)
{
	beginRemoveRows(parent, row, row + count - 1);
	for (int i = 0; i < count; i++)
		_variables.removeAt(row);
	endRemoveRows();
	return true;
}

bool ListModelVariables::insertRows(int row, int count, const QModelIndex &parent)
{
	beginInsertRows(parent, row, row + count - 1);
	for (int i = 0; i < count; i++)
		_variables.insert(row, ColumnInfo(QString(), 0));
	endInsertRows();
	return true;
}

Qt::ItemFlags ListModelVariables::flags(const QModelIndex &index) const
{
	if (index.isValid())
	{
		if (isForbidden(_variables.at(index.row()).second))
			return Qt::ItemNeverHasChildren;

		return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemNeverHasChildren | Qt::ItemIsDragEnabled;
	}
	else
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
}

Qt::DropActions ListModelVariables::supportedDropActions() const
{
	return _dropActions;
}

Qt::DropActions ListModelVariables::supportedDragActions() const
{
	return _dragActions;
}

QStringList ListModelVariables::mimeTypes() const
{
	QStringList types;

	types << _mimeType;

	return types;
}

QMimeData *ListModelVariables::mimeData(const QModelIndexList &indexes) const
{
	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << indexes.length();

	foreach (const QModelIndex &index, indexes)
	{
		if (index.isValid())
		{
			ColumnInfo info = _variables.at(index.row());
			dataStream << info;
		}
	}

	ListModelVariables* th1s = (ListModelVariables*) this;
	th1s->_mimeData = mimeData;

	mimeData->setData(_mimeType, encodedData);

	return mimeData;
}

bool ListModelVariables::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	if (_dragActions == Qt::CopyAction && _dropActions == Qt::MoveAction && action == Qt::MoveAction) // if delete
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	if (action == Qt::IgnoreAction)
		return true;

	if (data->hasFormat(_mimeType))
	{
		QByteArray encodedData = data->data(_mimeType);
		QDataStream stream(&encodedData, QIODevice::ReadOnly);
		QList<ColumnInfo> newItems;
		int rows = 0;

		if (stream.atEnd())
			return false;

		int count;
		stream >> count;

		while (!stream.atEnd())
		{
			ColumnInfo variable;
			stream >> variable;

			if (_variables.contains(variable))
				continue;

			newItems << variable;
			++rows;
		}

		if (rows == 0)
			return false;

		int beginRow;

		if (row != -1)
			beginRow = row;
		else if (parent.isValid())
			beginRow = parent.row();
		else
			beginRow = rowCount(QModelIndex());

		int r = 0;
		foreach (const ColumnInfo &item, _variables)
		{
			if (isForbidden(item.second))
				break;
			r++;
		}

		if (r < beginRow)
			beginRow = r;

		insertRows(beginRow, rows, QModelIndex());

		r = 0;
		foreach (const ColumnInfo &item, newItems)
			_variables[beginRow + r++] = item;

		emit dataChanged(index(beginRow, 0), index(beginRow + r, 0));

		return true;
	}

	return false;
}

bool ListModelVariables::canDropMimeData(const QMimeData *data, Qt::DropAction action, int, int, const QModelIndex &) const
{
	if (_dragActions == Qt::CopyAction && _dropActions == Qt::MoveAction && action == Qt::MoveAction) // if delete
		return true;

	if (data->hasFormat(_mimeType))
	{
		QByteArray encodedData = data->data(_mimeType);
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		if (stream.atEnd()) // is empty
			return false;

		int count;
		stream >> count;

		while (!stream.atEnd())
		{
			ColumnInfo variable;
			stream >> variable;

			if (isForbidden(variable.second))
				return false;
		}

		return true;
	}
	else
	{
		return false;
	}
}

void ListModelVariables::setSupportedDropActions(Qt::DropActions actions)
{
	_dropActions = actions;
}

void ListModelVariables::setSupportedDragActions(Qt::DropActions actions)
{
	_dragActions = actions;
}

void ListModelVariables::setMimeType(const QString &mimeType)
{
	_mimeType = mimeType;
}

bool ListModelVariables::isForbidden(int variableType) const
{
	return variableType & ~_variableTypesAllowed;
}

bool ListModelVariables::isDroppingToSelf(const QMimeData *mimeData) const
{
	return _mimeData == mimeData;
}
