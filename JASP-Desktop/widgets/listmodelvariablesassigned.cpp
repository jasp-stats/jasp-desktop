#include "listmodelvariablesassigned.h"

#include <vector>
#include <string>
#include <QMimeData>
#include <QTimer>

#include "options/optionfield.h"

using namespace std;

ListModelVariablesAssigned::ListModelVariablesAssigned(QObject *parent)
	: ListModelVariables(parent)
{
	_boundTo = NULL;
	_onlyOne = false;
	_source = NULL;

	connect(this, SIGNAL(dataChanged(QModelIndex,QModelIndex)), this, SLOT(assignToBoundOption()));
	connect(this, SIGNAL(rowsRemoved(const QModelIndex,int,int)), this, SLOT(assignToBoundOption()));
}

void ListModelVariablesAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionFields *>(option);

	if (dynamic_cast<OptionField *>(option) != NULL)
		_onlyOne = true;
}

bool ListModelVariablesAssigned::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	if (_onlyOne == false)
		return ListModelVariables::canDropMimeData(data, action, row, column, parent);

	if (mimeTypes().contains("application/vnd.list.variable"))
	{
		QByteArray encodedData = data->data("application/vnd.list.variable");
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		if (stream.atEnd()) // is empty
			return false;

		int count;

		stream >> count;

		if (count == 1)
			return ListModelVariables::canDropMimeData(data, action, row, column, parent);
	}

	return false;
}

bool ListModelVariablesAssigned::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	if (action == Qt::IgnoreAction)
		return true;

	if (canDropMimeData(data, action, row, column, parent) && (action == Qt::MoveAction || action == Qt::CopyAction))
	{
		if (_variables.length() > 0 && _onlyOne)
		{
			if (_source != NULL)
			{
				_toEject = _variables;
				QTimer::singleShot(50, this, SLOT(eject()));
			}
			else
			{
				qDebug() << "ListModelVariablesAssigned::dropMimeData() : No source set";
			}

			removeRows(0, 1, QModelIndex());
		}

		if (ListModelVariables::dropMimeData(data, action, row, column, parent))
		{
			emit assignmentsChanged();
			return true;
		}
	}

	return false;
}

bool ListModelVariablesAssigned::removeRows(int row, int count, const QModelIndex &parent)
{
	if (ListModelVariables::removeRows(row, count, parent))
	{
		emit assignmentsChanged();
		return true;
	}

	return false;
}

void ListModelVariablesAssigned::setSource(ListModelVariablesAvailable *source)
{
	_source = source;

	connect(source, SIGNAL(variablesChanged()), this, SLOT(sourceVariablesChanged()));
}

const QList<ColumnInfo> &ListModelVariablesAssigned::assigned() const
{
	return _variables;
}

void ListModelVariablesAssigned::assignToBoundOption()
{
	if (_boundTo != NULL)
	{
		vector<string> variables;
		foreach (ColumnInfo v, _variables)
		{
			QByteArray bytes = v.first.toUtf8();
			variables.push_back(string(bytes.constData(), bytes.length()));
		}
		_boundTo->setValue(variables);
	}
}

void ListModelVariablesAssigned::eject()
{
	qDebug() << "eject " << _toEject.first().first;

	_source->sendBack(_toEject);
	_toEject.clear();
}

void ListModelVariablesAssigned::sourceVariablesChanged()
{
	const QList<ColumnInfo> &variables = _source->allVariables();

	bool variableRemoved = false;

	foreach (ColumnInfo variable, _variables)
	{
		if ( ! variables.contains(variable))
		{
			if ( ! variableRemoved)
				beginResetModel();

			_variables.removeOne(variable);
			variableRemoved = true;
		}
	}

	if (variableRemoved)
	{
		assignToBoundOption();
		endResetModel();
	}

}
