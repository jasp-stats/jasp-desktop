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

#include "tablemodelanovawithinsubjectcells.h"

#include "terms.h"
#include "qutils.h"

#include <QSize>
#include <QMimeData>
#include <QTimer>

using namespace std;

TableModelAnovaWithinSubjectCells::TableModelAnovaWithinSubjectCells(QObject *parent) :
	TableModelVariables(parent)
{
	_boundTo = NULL;
	_source = NULL;
}

int TableModelAnovaWithinSubjectCells::rowCount(const QModelIndex &) const
{
	return _variables.size();
}

int TableModelAnovaWithinSubjectCells::columnCount(const QModelIndex &) const
{
	return 2;
}

QVariant TableModelAnovaWithinSubjectCells::data(const QModelIndex &index, int role) const
{
	if (role == Qt::DisplayRole)
	{
		if (index.column() == 0)
		{
			string value = _variables.at(index.row());
			if (value == "")
				return QVariant();
			else
				return tq(value);
		}
		else
		{
			if (_design.length() == 0)
				return QVariant();

			int value = index.row();
			int divisor = 1;

			const QStringList &levels = _design.last().second;
			int pos = (value / divisor) % levels.length();

			QString label = levels.at(pos);
			divisor *= levels.length();

			for (int i = _design.length() - 2; i >= 0; i--)
			{
				const QStringList &levels = _design.at(i).second;
				int pos = (value / divisor) % levels.length();
				label.prepend(", ").prepend(levels.at(pos));
				divisor *= levels.length();
			}

			return label;
		}
	}
	else if (role == Qt::DecorationRole)
	{
		if (index.column() == 0)
		{
			string value = _variables.at(index.row());
			if (value == "")
				return QVariant();
			else
				return requestIcon(value);
		}
		else
		{
			return QVariant();
		}
	}
	else if (role == Qt::SizeHintRole)
	{
		if (index.column() == 0)
			return QSize(180, 12);
		else
			return QSize(20, 12);
	}

	return QVariant();
}

Qt::ItemFlags TableModelAnovaWithinSubjectCells::flags(const QModelIndex &index) const
{
	if (index.column() == 0)
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled | (data(index).isNull() == false ? Qt::ItemIsSelectable | Qt::ItemIsDragEnabled : Qt::NoItemFlags);
	else
		return Qt::ItemIsEnabled;
}

QVariant TableModelAnovaWithinSubjectCells::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
	{
		return section == 0 ? "Variable" : "Cell";
	}
	else if (orientation == Qt::Vertical && role == Qt::SizeHintRole)
	{
		return QSize(0, 12);
	}

	return QVariant();
}

bool TableModelAnovaWithinSubjectCells::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	Q_UNUSED(row);

	int colNo = (column == -1 ? parent.column() : column);
	int rowNo = (row    == -1 ? parent.row()    : row);

	if (_boundTo == NULL)
		return false;

	if (action == Qt::IgnoreAction)
		return true;

	if ( ! data->hasFormat("application/vnd.list.variable"))
		return false;

	if (action != Qt::MoveAction)
		return false;

	if (colNo != 0)
		return false;

	QByteArray encodedData = data->data("application/vnd.list.variable");

	Terms dropped;
	dropped.set(encodedData);

	foreach (const Term &term, dropped)
	{
		if ( ! isAllowed(term))
			return false;
	}

	if (rowNo == -1)
	{
		size_t available = 0;

		for (size_t i = 0; i < _variables.size(); i++)
		{
			if (_variables.at(i) == "")
				available++;
		}

		return dropped.size() <= available;
	}
	else
	{
		return dropped.size() <= _variables.size() - rowNo;
	}

	return true;
}

bool TableModelAnovaWithinSubjectCells::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	Q_UNUSED(column);

	if (action == Qt::IgnoreAction)
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	//int colNo = (column == -1 ? parent.column() : column);
	int rowNo = (row    == -1 ? parent.row()    : row);

	QByteArray encodedData = data->data("application/vnd.list.variable");

	Terms dropped;
	dropped.set(encodedData);

	vector<string> droppedItems = dropped.asVector();

	beginResetModel();

	if (rowNo == -1)
	{
		size_t pos = 0;

		for (size_t i = 0; i < _variables.size(); i++)
		{
			if (_variables.at(i) == "")
				_variables[i] = droppedItems.at(pos++);

			if (pos >= droppedItems.size())
				break;
		}
	}
	else
	{
		size_t pos = 0;

		for (size_t i = rowNo; i < _variables.size(); i++)
		{
			string existingVariable = _variables.at(i);
			if (existingVariable != "")
				_toSendBack.add(existingVariable);

			_variables[i] = droppedItems[pos++];

			if (pos >= droppedItems.size())
				break;
		}

		if (_toSendBack.size() > 0)
			QTimer::singleShot(0, this, SLOT(sendBack()));
	}

	_boundTo->setValue(_variables);

	endResetModel();

	return true;
}

Qt::DropActions TableModelAnovaWithinSubjectCells::supportedDropActions() const
{
	return Qt::MoveAction;
}

Qt::DropActions TableModelAnovaWithinSubjectCells::supportedDragActions() const
{
	return Qt::MoveAction;
}

QStringList TableModelAnovaWithinSubjectCells::mimeTypes() const
{
	QStringList types;

	types << "application/vnd.list.variable";

	return types;
}

QMimeData *TableModelAnovaWithinSubjectCells::mimeData(const QModelIndexList &indexes) const
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
			dataStream << term.components();
		}
	}

	mimeData->setData("application/vnd.list.variable", encodedData);

	return mimeData;
}

void TableModelAnovaWithinSubjectCells::mimeDataMoved(const QModelIndexList &indexes)
{
	beginResetModel();

	foreach (const QModelIndex index, indexes)
		_variables[index.row()] = "";

	endResetModel();

	_boundTo->setValue(_variables);
}

void TableModelAnovaWithinSubjectCells::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariables*>(option);

	if (_boundTo != NULL)
	{
		_variables = _boundTo->variables();
		setDesign(_design);
		_source->notifyAlreadyAssigned(_variables);
	}
}

void TableModelAnovaWithinSubjectCells::unbind()
{
	beginResetModel();

	_boundTo = NULL;

	endResetModel();
}

void TableModelAnovaWithinSubjectCells::setSource(TableModelVariablesAvailable *source)
{
	_source = source;
	setInfoProvider(source);
}

void TableModelAnovaWithinSubjectCells::setDesign(const QList<Factor> &design)
{
	beginResetModel();

	_design = design;

	if (_boundTo != NULL)
	{
		size_t designCells = designCellCount();

		if (_variables.size() > designCells)
		{
			vector<string>::iterator itr = _variables.begin();
			for (size_t i = 0; i < designCells; i++)
				itr++;

			vector<string>::iterator jtr = itr;
			for (; jtr != _variables.end(); jtr++)
			{
				if (*jtr != "")
					_toSendBack.add(*jtr);
			}

			_variables.erase(itr, _variables.end());

			QTimer::singleShot(0, this, SLOT(sendBack()));

			_boundTo->setValue(_variables);
		}
		else if (_variables.size() < designCells)
		{
			do
			{
				_variables.push_back("");	
			}
			while (_variables.size() < designCells);

			_boundTo->setValue(_variables);
		}
	}

	endResetModel();
}

int TableModelAnovaWithinSubjectCells::slotsAvailable() const
{
	size_t available = 0;

	for (size_t i = 0; i < _variables.size(); i++)
	{
		if (_variables.at(i) == "")
			available++;
	}

	return available;
}

int TableModelAnovaWithinSubjectCells::designCellCount() const
{
	if (_design.length() == 0)
		return 0;

	int rows = 1;

	foreach (const Factor &factor, _design)
		rows *= factor.second.length();

	return rows;
}

void TableModelAnovaWithinSubjectCells::sendBack()
{
	if (_source != NULL)
	{
		_source->sendBack(_toSendBack);
		_toSendBack.clear();
	}
}
