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
}

void ListModelVariablesAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionFields *>(option);

	if (_boundTo != NULL)
	{
		if (dynamic_cast<OptionField *>(option) != NULL)
			_onlyOne = true;

		if (_source != NULL)
		{
			const vector<string> assigned = _boundTo->value();
			const QList<ColumnInfo> &allVariables = _source->allVariables();

			beginResetModel();

			_variables.clear();

			foreach (const string &value, assigned)
			{
				QString utf8 = QString::fromUtf8(value.c_str(), value.length());

				foreach (const ColumnInfo &info, allVariables)
				{
					if (info.first == utf8)
						_variables.append(info);
				}
			}

			endResetModel();

			_source->notifyAlreadyAssigned(_variables);
		}
		else
		{
			qDebug() << "ListModelVariablesAssigned::bindTo(); source not set";
		}
	}
	else
	{
		qDebug() << "ListModelVariablesAssigned::bindTo(); option not of type OptionFields*";
	}
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
		if (_onlyOne)
		{
			if (_source != NULL)
			{
				QByteArray encodedData = data->data(_mimeType);
				QDataStream stream(&encodedData, QIODevice::ReadOnly);

				if (stream.atEnd())
					return false;

				int count;
				stream >> count;

				if (stream.atEnd())
					return false;

				ColumnInfo variable;
				stream >> variable;

				if (_variables.length() > 0)
				{
					_source->sendBack(_variables);

					_variables.clear();
					_variables.append(variable);

					emit dataChanged(index(0), index(0));
				}
				else
				{
					beginInsertRows(parent, 0, 0);
					_variables.append(variable);
					endInsertRows();
				}

			}
			else
			{
				qDebug() << "ListModelVariablesAssigned::dropMimeData() : No source set";
			}
		}
		else
		{
			ListModelVariables::dropMimeData(data, action, row, column, parent);
		}

		emit assignmentsChanged();
		assignToBoundOption();

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

void ListModelVariablesAssigned::mimeDataMoved(const QModelIndexList &indexes)
{
	ListModelVariables::mimeDataMoved(indexes);
	emit assignmentsChanged();
	assignToBoundOption();
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
		emit assignmentsChanged();
		endResetModel();
	}

}
