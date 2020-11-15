//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "listmodelinputvalue.h"
#include "analysis/options/options.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "utilities/qutils.h"
#include "log.h"

ListModelInputValue::ListModelInputValue(JASPListControl* listView, int minRows)
	: ListModel(listView), _minRows(minRows)
{
	setTermsAreVariables(false);
}


int ListModelInputValue::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	return int(_terms.size()) + (_addVirtual ? 1 : 0);
}

QVariant ListModelInputValue::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	size_t rowU = size_t(row);
	
	if (row >= rowCount())
	{
		Log::log()  << "Unknown row " << row << " in ListModelFactors. Length is " << rowCount() << std::endl;
		return QVariant();
	}

	QString value;
	if (role == Qt::DisplayRole || role == ListModelInputValue::NameRole)
	{
		QString value;
		if (rowU < _terms.size())
			value = _terms.at(rowU).asQString();
		else if (_addVirtual)
			value = _placeholder;
		return value;
	}
	else if (role == ListModelInputValue::TypeRole)
	{
		bool isVirtual = (_addVirtual && rowU == _terms.size());
		QStringList listValues;
		if (isVirtual)
			listValues.push_back(tq("virtual"));

		if (row >= _minRows && !isVirtual)
			listValues.push_back(tq("deletable"));
		value = listValues.join(',');
		return value;
	}
	else
		return ListModel::data(index, role);
}

void ListModelInputValue::_removeTerm(int row)
{
	QString value;
	size_t rowU = size_t(row);

	if (rowU >= _terms.size())
		return;

	beginRemoveRows(QModelIndex(), row, row);
	_terms.remove(rowU);
	endRemoveRows();
}

QString ListModelInputValue::_changeLastNumber(const QString &val)
{
	QString result = val;
	int index = val.length() - 1;
	for (; index >= 0 ; index--)
	{
		if (!val.at(index).isDigit())
			break;
	}
	index++;

	int num = -1;
	if (index >= 0 && index < val.length())
	{
		bool ok = false;
		num = val.right(val.length() - index).toInt(&ok);
		if (!ok)
			num = -1;
	}

	if (num >= 0)
		return result.left(index).append(QString::number(num + 1));
	else
		return result.append(QString::number(1));
}

QString ListModelInputValue::_makeUnique(const QString &val, int row)
{
	QString result = val;
	QList<QString> values = _terms.asQList();
	bool isUnique = true;
	do
	{
		int i = 0;
		isUnique = true;
		for (const QString& value : values)
		{
			if (i != row)
			{
				if (value == result)
				{
					isUnique = false;
					result = _changeLastNumber(result);
				}
			}
			i++;
		}
	} while (!isUnique);

	return result;
}


void ListModelInputValue::itemChanged(int row, QVariant value)
{
	if (row < 0)
		return;
	if (row >= rowCount())
	{
		Log::log()  << "Index " << row << " in ListModelFactors is greater than the maximum " << rowCount() << std::endl;
		return;
	}

	QString val = value.toString();
	size_t rowU = size_t(row);

	bool isVirtual = (_addVirtual && rowU == _terms.size());
	if (isVirtual)
	{
		if (val.isEmpty())
			return;
		beginResetModel();
		_terms.add(_makeUnique(val));
		endResetModel();
	}
	else
	{
		if (val.isEmpty() && row < _minRows)
			val = "1";

		if (val.isEmpty())
			_removeTerm(row);
		else
		{
			beginResetModel();
			val = _makeUnique(val, row);
			QList<QString> values = _terms.asQList();
			values[row] = val;
			_terms.set(values);
			endResetModel();
		}
	}

	emit modelChanged();
}

void ListModelInputValue::itemRemoved(int row)
{
	_removeTerm(row);
	emit modelChanged();
}
