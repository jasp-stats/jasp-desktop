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

#include "tablemodelvariablesassigned.h"

#include <vector>
#include <string>
#include <QMimeData>
#include <QTimer>
#include <QDebug>

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
	_boundTo = dynamic_cast<OptionTerms *>(option);

	if (_boundTo != NULL)
	{
		if (_source != NULL)
		{
			const vector<vector<string> > assigned = _boundTo->value();

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

	if (isDroppingToSelf(data))
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

	_delayDropped.set(encodedData);
	QTimer::singleShot(0, this, SLOT(delayAssignDroppedData()));

	emit assignmentsChanging();

	return true;
}

void TableModelVariablesAssigned::delayAssignDroppedData()
{
	assign(_delayDropped);

	emit assignmentsChanged();
}

void TableModelVariablesAssigned::mimeDataMoved(const QModelIndexList &indices)
{
	emit assignmentsChanging();

	Terms variablesToRemove;

	foreach (const QModelIndex &index, indices)
		variablesToRemove.add(_variables.at(index.row()));

	unassign(variablesToRemove);

	emit assignmentsChanged();
}

void TableModelVariablesAssigned::setSorted(bool sorted)
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
	emit assignmentsChanging();

	const Terms &variables = _source->allVariables();
	Terms variablesToKeep;
	bool variableRemoved = false;

	variablesToKeep.set(_variables);
	variableRemoved = variablesToKeep.discardWhatDoesntContainTheseComponents(variables);

	if (variableRemoved)
		setAssigned(variablesToKeep);

	emit assignmentsChanged();
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

	emit assignedTo(variables);
}

void TableModelVariablesAssigned::unassign(const Terms &variables)
{
	Terms variablesToKeep;
	variablesToKeep.set(_variables);
	variablesToKeep.remove(variables);
	setAssigned(variablesToKeep);

	emit unassigned(variables);
}

void TableModelVariablesAssigned::setAssigned(const Terms &variables)
{
	if (_source == NULL)
	{
		qDebug() << "TableModelVariablesAssigned::setAssigned() : Source not set!";
		return;
	}

	beginResetModel();
	_variables.set(variables);
	endResetModel();

	if (_boundTo != NULL)
		_boundTo->setValue(_variables.asVectorOfVectors());
}

void TableModelVariablesAssigned::sendBack()
{
	_source->sendBack(_toSendBack);
	_toSendBack.clear();
}
