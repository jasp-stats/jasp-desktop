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

#include "listmodelpairsassigned.h"

#include <QDebug>

using namespace std;

ListModelPairsAssigned::ListModelPairsAssigned(AnalysisQMLForm *form, QQuickItem* item)
	: ListModelAssigned(form, item)
{
	_boundTo = NULL;
	_source = NULL;
	_removeTermsWhenDropped = true;
	_variableTypesSuggested = Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale;
}

void ListModelPairsAssigned::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionVariablesGroups *>(option);

	if (_boundTo == NULL)
	{
		qDebug() << "ListModelPairAssigned::bindTo(); Could not bind to option";
		return;
	}

	if (_source == NULL)
	{
		qDebug() << "ListModelPairAssigned::bindTo(); source not set";
		return;
	}

	beginResetModel();

	_values = _boundTo->value();

	endResetModel();
}

void ListModelPairsAssigned::setSource(ListModelAvailable *source)
{
	ListModelAssigned::setSource(source);
}

int ListModelPairsAssigned::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	int size = _values.size();
	return size * 2;
}

QVariant ListModelPairsAssigned::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
	{
		qDebug() << "Data invalid!";
		return QVariant();
	}

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		int indexRow = index.row();
		int realRow = indexRow / 2;
		int realCol = indexRow % 2;
		const Term &row = _values.at(realRow);
		QString result = row.at(realCol);
		return result;
	}
	else
	{
		qDebug() << "Unused Role: " << role;
	}

	return QVariant();
}

Terms *ListModelPairsAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Q_UNUSED(indexes);
	return new Terms();
}

const Terms &ListModelPairsAssigned::terms() const
{
	return _values;
}

void ListModelPairsAssigned::removeTermsAfterBeingDropped(const QList<int> &indexes)
{
	beginResetModel();

	QSet<int> rows;
	for (const int &row: indexes)
		rows.insert(row/2);

	QList<int> lrows = rows.toList();
	std::sort(lrows.begin(), lrows.end(), qGreater<int>());

	for (const int &row: lrows)
		_values.remove(row);

	endResetModel();

	assignToOption();
}

bool ListModelPairsAssigned::canDropTerms(const Terms *terms) const
{
	for (const Term &variable : *terms)
	{
		if ( ! isAllowed(variable))
			return false;
	}

	return true;
}

bool ListModelPairsAssigned::dropTerms(const Terms *terms)
{
	if ( ! canDropTerms(terms))
		return false;

	beginResetModel();
	for (const Term& term: *terms)
	{
		const QString& variableName = term.asQString();
		if (_values.size() == 0)
		{
			QStringList newRow;
			newRow.push_back(variableName);
			newRow.push_back(QString());
			_values.add(Term(newRow));
		}
		else
		{
			QStringList newRow;
			const QStringList& lastRow = _values.at(_values.size() - 1).components();
			if (lastRow[1].isEmpty())
			{
				newRow.push_back(lastRow[0]);				
				newRow.push_back(variableName);
				_values.remove(_values.size() - 1);
			}
			else
			{
				newRow.push_back(variableName);
				newRow.push_back(QString());				
			}
			_values.add(Term(newRow));
		}
	}
	endResetModel();

	assignToOption();

	return true;
}

void ListModelPairsAssigned::assignToOption()
{
	if (_boundTo != NULL)
	{
		vector<vector<string> > pairs;

		for (const Term &qPair : _values)
		{
			vector<string> pair;
			pair.push_back(qPair.at(0).toStdString());
			pair.push_back(qPair.at(1).toStdString());
			pairs.push_back(pair);
		}

		_boundTo->setValue(pairs);
	}

}


