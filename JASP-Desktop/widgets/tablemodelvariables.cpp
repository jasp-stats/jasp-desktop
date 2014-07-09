#include "tablemodelvariables.h"

#include <QMimeData>

#include "column.h"

#include <QDebug>

TableModelVariables::TableModelVariables(QObject *parent) :
	TableModel(parent)
{
	_variableTypesSuggested = 0;
	_variableTypesAllowed = 0xff;

	_nominalTextIcon = QIcon(":/icons/variable-nominal-text.svg");
	_nominalIcon = QIcon(":/icons/variable-nominal.svg");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
	_scaleIcon = QIcon(":/icons/variable-scale.svg");

	_dropActions = Qt::MoveAction;
	_dragActions = Qt::MoveAction;

	_defaultTarget = NULL;

	_mimeType = "application/vnd.list.variable";
}

void TableModelVariables::setVariableTypesSuggested(int variableTypesSuggested)
{
	_variableTypesSuggested = variableTypesSuggested;
}

int TableModelVariables::variableTypesSuggested() const
{
	return _variableTypesSuggested;
}

void TableModelVariables::setVariableTypesAllowed(int variableTypesAllowed)
{
	_variableTypesAllowed = variableTypesAllowed;
}

int TableModelVariables::variableTypesAllowed() const
{
	return _variableTypesAllowed;
}

int TableModelVariables::rowCount(const QModelIndex &) const
{
	return _variables.size();
}

int TableModelVariables::columnCount(const QModelIndex &parent) const
{
	return 1;
}

QVariant TableModelVariables::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole)
	{
		Term term = _variables.at(row);
		return QVariant(term.asQString());
	}
	else if (role == Qt::DecorationRole)
	{
		Term term = _variables.at(row);
		int variableType = requestInfo(term, VariableInfo::VariableType).toInt();

		switch (variableType)
		{
		case Column::ColumnTypeNominalText:
			return QVariant(_nominalTextIcon);
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

bool TableModelVariables::insertRows(int row, int count, const QModelIndex &parent)
{
	/*beginInsertRows(parent, row, row + count - 1);
	for (int i = 0; i < count; i++)
		_variables.insert(row, ColumnInfo(QString(), 0));
	endInsertRows();*/
	return true;
}

Qt::ItemFlags TableModelVariables::flags(const QModelIndex &index) const
{
	if (index.isValid())
	{
		const Term &term = _variables.at(index.row());
		if ( ! isAllowed(term))
            return 0;

        return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsDragEnabled;
	}
	else
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
}

Qt::DropActions TableModelVariables::supportedDropActions() const
{
	return _dropActions;
}

Qt::DropActions TableModelVariables::supportedDragActions() const
{
	return _dragActions;
}

QStringList TableModelVariables::mimeTypes() const
{
	QStringList types;

	types << _mimeType;

	return types;
}

QMimeData *TableModelVariables::mimeData(const QModelIndexList &indexes) const
{
	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << indexes.length();

	foreach (const QModelIndex &index, indexes)
	{
		if (index.isValid())
		{
			Term term = _variables.at(index.row());
			dataStream << term.asQString();
		}
	}

	TableModelVariables* th1s = (TableModelVariables*) this;
	th1s->_mimeData = mimeData;

	mimeData->setData(_mimeType, encodedData);

	return mimeData;
}

void TableModelVariables::mimeDataMoved(const QModelIndexList &indexes)
{
	beginResetModel();

	QModelIndexList sorted = indexes;

	qSort(sorted.begin(), sorted.end(), qGreater<QModelIndex>());

	foreach (const QModelIndex &index, sorted)
		_variables.remove(index.row());

	endResetModel();
}

bool TableModelVariables::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
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

		Terms variables;
		variables.set(encodedData);

		beginResetModel();
		_variables.add(variables);
		endResetModel();

		return true;
	}

	return false;
}

bool TableModelVariables::canDropMimeData(const QMimeData *data, Qt::DropAction action, int, int, const QModelIndex &) const
{
	if (_dragActions == Qt::CopyAction && _dropActions == Qt::MoveAction && action == Qt::MoveAction) // if delete
		return true;

	if (data->hasFormat(_mimeType))
	{
		QByteArray encodedData = data->data(_mimeType);

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

void TableModelVariables::setSupportedDropActions(Qt::DropActions actions)
{
	_dropActions = actions;
}

void TableModelVariables::setSupportedDragActions(Qt::DropActions actions)
{
	_dragActions = actions;
}

void TableModelVariables::setMimeType(const QString &mimeType)
{
	_mimeType = mimeType;
}

bool TableModelVariables::isSuggested(const Term &term) const
{
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	int variableType = v.toInt();

	return variableType & _variableTypesSuggested;
}

bool TableModelVariables::isAllowed(const Term &term) const
{
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	int variableType = v.toInt();

	return variableType == 0 || variableType & _variableTypesAllowed;
}

bool TableModelVariables::isDroppingToSelf(const QMimeData *mimeData) const
{
	return _mimeData == mimeData;
}
