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

#include "tablemodelcontrasts.h"

#include "options/optionvariables.h"

#include <QColor>
#include <QBrush>

using namespace std;

TableModelContrasts::TableModelContrasts(QObject *parent) :
	TableModel(parent)
{
	_boundTo = NULL;
}

void TableModelContrasts::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
}

void TableModelContrasts::setLabels(const Terms &levels)
{
	if (_boundTo == NULL)
		return;

	beginResetModel();

	_labels = levels;

	for (uint i = 0; i < _contrasts.size(); i++)
		delete _contrasts.at(i);
	_contrasts.clear();

	_boundTo->setValue(_contrasts);

	endResetModel();
}

int TableModelContrasts::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_boundTo == NULL)
		return 0;

	return (int)_labels.size();
}

int TableModelContrasts::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_boundTo == NULL)
		return 0;

	return 1 + (int)_contrasts.size() + 1;
}

QVariant TableModelContrasts::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	int contrastIndex = index.column() - 1;

	if (contrastIndex == -1)
	{
		if (role == Qt::DisplayRole)
			return _labels.at(index.row()).asQString();
	}
	else if (contrastIndex == (int)_contrasts.size())
	{
		if (role == Qt::DisplayRole)
		{
			switch (index.row())
			{
			case 0:
				return "A";
			case 1:
				return "B";
			default:
				return "";
			}
		}
		else if (role == Qt::ForegroundRole)
		{
			return QBrush(QColor(0xAA, 0xAA, 0xAA));
		}
		else if (role == Qt::TextAlignmentRole)
		{
			return Qt::AlignCenter;
		}
		else if (role == Qt::SizeHintRole)
		{
			return QSize(40, -1);
		}
	}
	else
	{
		if (role == Qt::DisplayRole)
		{
			Term label = _labels.at(index.row());
			Options *contrast = _contrasts.at(contrastIndex);

			OptionVariables *A = dynamic_cast<OptionVariables *>(contrast->get("A"));
			Terms Acontents(A->variables());
			if (Acontents.contains(label))
				return "A";

			OptionVariables *B = dynamic_cast<OptionVariables *>(contrast->get("B"));
			Terms Bcontents(B->variables());
			if (Bcontents.contains(label))
				return "B";

			return "";
		}
		else if (role == Qt::TextAlignmentRole)
		{
			return Qt::AlignCenter;
		}
		else if (role == Qt::SizeHintRole)
		{
			return QSize(40, -1);
		}
	}

	return QVariant();
}

bool TableModelContrasts::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo == NULL)
		return false;

	if (role != Qt::DisplayRole)
		return false;

	int labelIndex = index.row();
	int contrastIndex = index.column() - 1;

	if (contrastIndex < 0)
		return false;

	if (contrastIndex == (int)_contrasts.size())
	{
		beginInsertColumns(index.parent(), index.column(), index.column());
		Options *newContrast = static_cast<Options *>(_boundTo->rowTemplate()->clone());
		_contrasts.push_back(newContrast);
		endInsertColumns();
	}

	Term label = _labels.at(labelIndex);
	Options *contrast = _contrasts.at(contrastIndex);
	QString newValue = value.toString();

	OptionVariables *A = dynamic_cast<OptionVariables *>(contrast->get("A"));
	OptionVariables *B = dynamic_cast<OptionVariables *>(contrast->get("B"));

	Terms Acontents(A->variables(), &_labels);
	Terms Bcontents(B->variables(), &_labels);

	if (newValue == "A")
	{
		Acontents.add(label);
		Bcontents.remove(label);
	}
	else if (newValue == "B")
	{
		Acontents.remove(label);
		Bcontents.add(label);
	}
	else
	{
		Acontents.remove(label);
		Bcontents.remove(label);
	}

	if (Acontents.size() == 0 && Bcontents.size() == 0)
	{
		beginRemoveColumns(index.parent(), index.column(), index.column());
		_contrasts.erase(find(_contrasts.begin(), _contrasts.end(), contrast));
		delete contrast;
		endRemoveColumns();
	}
	else
	{
		A->setValue(Acontents.asVector());
		B->setValue(Bcontents.asVector());
		emit dataChanged(index, index, QVector<int>(1, Qt::DisplayRole));
	}

	_boundTo->setValue(_contrasts);

	return true;
}

Qt::ItemFlags TableModelContrasts::flags(const QModelIndex &index) const
{
	Q_UNUSED(index);

	return Qt::ItemIsEnabled;
}

QVariant TableModelContrasts::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (orientation == Qt::Horizontal)
	{
		if (role == Qt::DisplayRole)
		{
			if (section == 0)
				return "Labels";

			return QString("Contrast %1").arg(section);
		}
	}

	return QVariant();
}
