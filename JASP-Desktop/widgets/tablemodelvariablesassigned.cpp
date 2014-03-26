
#include "tablemodelvariablesassigned.h"
#include "column.h"

#include <QMimeData>
#include <QTimer>
#include <QDebug>

using namespace std;

TableModelVariablesAssigned::TableModelVariablesAssigned(QObject *parent)
	: TableModel(parent)
{
	_boundTo = NULL;
	_source = NULL;

	_variableTypesAllowed = Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale;
}

void TableModelVariablesAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionFieldPairs *>(option);

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

	vector<pair<string, string> > assigned = _boundTo->value();
	const QList<ColumnInfo> &allVariables = _source->allVariables();

	beginResetModel();

	_values.clear();

	pair<string, string> p;

	foreach (p, assigned)
	{
		ColumnInfo info1;
		ColumnInfo info2;

		string first = p.first;
		string second = p.second;

		QString firstQ = QString::fromUtf8(first.c_str(), first.size());
		QString secondQ = QString::fromUtf8(second.c_str(), second.size());

		foreach (const ColumnInfo &info, allVariables)
		{
			if (info.first == firstQ)
			{
				info1.first = info.first;
				info1.second = info.second;
			}

			if (info.first == secondQ)
			{
				info2.first = info.first;
				info2.second = info.second;
			}

			if (info1.first != "" && info2.first != "")
				break;
		}

		VarPair newPair;
		newPair.append(info1);
		newPair.append(info2);

		_values.append(newPair);
	}

	endResetModel();
}

void TableModelVariablesAssigned::setSource(ListModelVariablesAvailable *source)
{
	_source = source;
}

int TableModelVariablesAssigned::rowCount(const QModelIndex &parent) const
{
	return _values.length();
}

int TableModelVariablesAssigned::columnCount(const QModelIndex &parent) const
{
	return 2;
}

QVariant TableModelVariablesAssigned::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
		return QVariant();

	if (role == Qt::DisplayRole)
		return QVariant(_values.at(index.row()).at(index.column()).first);

	return QVariant();
}

Qt::ItemFlags TableModelVariablesAssigned::flags(const QModelIndex &index) const
{	
	if (index.isValid())
		return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsDragEnabled | Qt::ItemIsDropEnabled;
	else
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
}

Qt::DropActions TableModelVariablesAssigned::supportedDropActions() const
{
	return Qt::CopyAction;
}

Qt::DropActions TableModelVariablesAssigned::supportedDragActions() const
{
	return Qt::MoveAction;
}

QStringList TableModelVariablesAssigned::mimeTypes() const
{
	QStringList types;

	types << "application/vnd.list.variable";

	return types;
}

QMimeData *TableModelVariablesAssigned::mimeData(const QModelIndexList &indexes) const
{
	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << 0;

	mimeData->setData("application/vnd.list.variable", encodedData);

	return mimeData;
}

bool TableModelVariablesAssigned::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
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

		ColumnInfo item1;
		stream >> item1;

		if (parent.isValid()) // drop into cell
		{
			_values[parent.row()][parent.column()] = item1;
			emit dataChanged(parent, parent);
		}
		else if (row == -1 && (!_values.isEmpty()) && _values.last().last().first == "")
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
				QList<ColumnInfo> newRow;
				newRow.append(item1);
				newRow.append(ColumnInfo("", 0));

				_values.insert(beginRow, newRow);
			}
			else
			{
				ColumnInfo item2;
				stream >> item2;

				QList<ColumnInfo> newRow;
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

bool TableModelVariablesAssigned::canDropMimeData(const QMimeData *data, Qt::DropAction, int row, int column, const QModelIndex &parent) const
{
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

bool TableModelVariablesAssigned::isForbidden(int variableType) const
{
	return variableType & ~_variableTypesAllowed;
}

bool TableModelVariablesAssigned::insertRows(int row, int count, const QModelIndex &parent)
{
	beginInsertRows(parent, row, row + count - 1);
	for (int i = 0; i < count; i++)
	{
		QList<ColumnInfo> newRow;
		newRow.append(ColumnInfo());
		newRow.append(ColumnInfo());
		_values.insert(row, newRow);
	}
	endInsertRows();

	return true;
}

void TableModelVariablesAssigned::mimeDataMoved(const QModelIndexList &indexes)
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

void TableModelVariablesAssigned::assignToOption()
{
	if (_boundTo != NULL)
		_boundTo->setValue(asVector(_values));
}

void TableModelVariablesAssigned::setVariableTypesAllowed(int variableTypesAllowed)
{
	_variableTypesAllowed = variableTypesAllowed;
}

int TableModelVariablesAssigned::variableTypesAllowed()
{
	return _variableTypesAllowed;
}

vector<pair<string, string> > TableModelVariablesAssigned::asVector(QList<VarPair> values)
{
	vector<pair<string, string> > vec;

	foreach (VarPair p, values)
	{
		QByteArray a1 = p.at(0).first.toUtf8();
		QByteArray a2 = p.at(1).first.toUtf8();
		string s1(a1.constData(), a1.length());
		string s2(a2.constData(), a2.length());

		vec.push_back(pair<string, string>(s1, s2));
	}

	return vec;
}

