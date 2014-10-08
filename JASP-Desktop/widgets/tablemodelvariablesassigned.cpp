#include "tablemodelvariablesassigned.h"

#include <vector>
#include <string>
#include <QMimeData>
#include <QTimer>
#include <QDebug>

#include "options/optionvariables.h"

using namespace std;

TableModelVariablesAssigned::TableModelVariablesAssigned(QObject *parent)
	: TableModelVariables(parent)
{
	_boundTo = NULL;
	_source = NULL;
	_sorted = false;
}

void TableModelVariablesAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariables *>(option);

	if (_boundTo != NULL)
	{
		if (_source != NULL)
		{
			const vector<string> assigned = _boundTo->variables();

			beginResetModel();
			_variables.set(assigned);
			endResetModel();

			_source->notifyAlreadyAssigned(_variables);
		}
		else
		{
			qDebug() << "TableModelVariablesAssigned::bindTo(); source not set";
		}
	}
	else
	{
		qDebug() << "TableModelVariablesAssigned::bindTo(); option not of type OptionVariables*";
	}
}

void TableModelVariablesAssigned::unbind()
{
	_boundTo = NULL;
}

void TableModelVariablesAssigned::setSource(TableModelVariablesAvailable *source)
{
	_source = source;
	setInfoProvider(source);

	if (_sorted)
		_variables.setSortParent(_source->allVariables());

	connect(source, SIGNAL(variablesChanged()), this, SLOT(sourceVariablesChanged()));
}

bool TableModelVariablesAssigned::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	if (_boundTo == NULL)
		return false;

	if ( ! TableModelVariables::canDropMimeData(data, action, row, column, parent))
		return false;

	if (_boundTo->onlyOneTerm())
	{
		QByteArray encodedData = data->data(_mimeType);

		Terms variables;
		variables.set(encodedData);

		if (variables.size() != 1)
			return false;
	}

	return true;
}

bool TableModelVariablesAssigned::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	if (_boundTo == NULL)
		return false;

	if (action == Qt::IgnoreAction)
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	if (action != Qt::MoveAction && action != Qt::CopyAction)
		return false;

	QByteArray encodedData = data->data(_mimeType);

	Terms variables;
	variables.set(encodedData);
	assign(variables);

	return true;
}

void TableModelVariablesAssigned::mimeDataMoved(const QModelIndexList &indices)
{
	Terms variablesToRemove;

	foreach (const QModelIndex &index, indices)
		variablesToRemove.add(_variables.at(index.row()));

	unassign(variablesToRemove);
}

bool TableModelVariablesAssigned::setSorted(bool sorted)
{
	_sorted = sorted;

	if (sorted && _source != NULL)
		_variables.setSortParent(_source->allVariables());
}

const Terms &TableModelVariablesAssigned::assigned() const
{
	return _variables;
}

void TableModelVariablesAssigned::sourceVariablesChanged()
{
	const Terms &variables = _source->allVariables();
	Terms variablesToKeep;
	bool variableRemoved = false;

	variablesToKeep.set(_variables);
	variableRemoved = variablesToKeep.discardWhatDoesntContainTheseComponents(variables);

	if (variableRemoved)
		setAssigned(variablesToKeep);
}

void TableModelVariablesAssigned::assign(const Terms &variables)
{
	if (_boundTo == NULL)
		return;

	Terms v;

	if (_boundTo->onlyOneTerm())
	{
		if (variables.size() > 0)
			v.add(variables.at(0));

		if (_variables.size() > 0)
		{
			_toSendBack.set(_variables);
			_variables.clear();
			QTimer::singleShot(0, this, SLOT(sendBack()));
		}
	}
	else
	{
		v.set(_variables);
		v.add(variables);
	}

	setAssigned(v);
}

void TableModelVariablesAssigned::unassign(const Terms &variables)
{
	Terms variablesToKeep;
	variablesToKeep.set(_variables);
	variablesToKeep.remove(variables);
	setAssigned(variablesToKeep);
}

void TableModelVariablesAssigned::setAssigned(const Terms &variables)
{
	if (_source == NULL)
	{
		qDebug() << "TableModelVariablesAssigned::setAssigned() : Source not set!";
		return;
	}

	if (_boundTo == NULL)
	{
		qDebug() << "TableModelVariablesAssigned::setAssigned() : Not bound!";
		return;
	}

	beginResetModel();
	_variables.set(variables);
	endResetModel();

	emit assignmentsChanged();

	if (_boundTo != NULL)
		_boundTo->setValue(_variables.asVector());
}

void TableModelVariablesAssigned::sendBack()
{
	_source->sendBack(_toSendBack);
	_toSendBack.clear();
}
