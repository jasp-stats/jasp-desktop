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

#include "tablemodelvariablesavailable.h"

#include "column.h"

#include <QDebug>
#include "qutils.h"

using namespace std;

TableModelVariablesAvailable::TableModelVariablesAvailable(QObject *parent)
	: TableModelVariables(parent)
{
}

void TableModelVariablesAvailable::setVariables(const Terms &variables)
{	
	beginResetModel();

	Terms suggested;
	Terms allowed;
	Terms forbidden;

	foreach (const Term &term, variables)
	{
		if ( ! isAllowed(term))
			forbidden.add(term);
		else if (isSuggested(term))
			suggested.add(term);
		else
			allowed.add(term);
	}
	Terms ordered; // present them in a nice order

	ordered.add(suggested);
	ordered.add(allowed);
	ordered.add(forbidden);

	_allVariables.set(ordered);
	_variables.removeParent();
	_variables.set(ordered);

	_variables.setSortParent(_allVariables);

	endResetModel();

	emit variablesChanged();
}

bool TableModelVariablesAvailable::removeRows(int row, int count, const QModelIndex &parent)
{
	beginRemoveRows(parent, row, row + count - 1);
	_variables.remove(row, count);
	endRemoveRows();
	return true;
}

QVariant TableModelVariablesAvailable::requestInfo(const Term &term, VariableInfo::InfoType info) const
{
	return VariableInfoConsumer::requestInfo(term, info);
}

bool TableModelVariablesAvailable::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	if (action == Qt::IgnoreAction)
		return true;

	if (isDroppingToSelf(data))
		return false;

	return TableModelVariables::canDropMimeData(data, action, row, column, parent);
}

QStringList TableModelVariablesAvailable::mimeTypes() const
{
	return TableModelVariables::mimeTypes();
}

const Terms &TableModelVariablesAvailable::allVariables() const
{
	return _allVariables;
}

void TableModelVariablesAvailable::notifyAlreadyAssigned(const Terms &variables)
{
	if (supportedDragActions() & Qt::CopyAction)
		return;

	beginResetModel();
	_variables.remove(variables);
	endResetModel();
}

void TableModelVariablesAvailable::sendBack(Terms &variables)
{
	beginResetModel();
	_variables.add(variables);
	endResetModel();
}

