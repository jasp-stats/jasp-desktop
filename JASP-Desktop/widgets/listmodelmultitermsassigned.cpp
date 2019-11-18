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

#include "listmodelmultitermsassigned.h"
#include "log.h"


using namespace std;

ListModelMultiTermsAssigned::ListModelMultiTermsAssigned(QMLListView* listView, int columns)
	: ListModelAssignedInterface(listView)
	, _columns(columns)
{
	_copyTermsWhenDropped = true;
}

int ListModelMultiTermsAssigned::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	int size = int(_terms.size());
	return size * _columns;
}

QVariant ListModelMultiTermsAssigned::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
	{
		Log::log()  << "ListModelPairsAssigned::data: Data invalid!" << std::endl;
		return QVariant();
	}

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		int indexRow = index.row();
		int realRow = indexRow / _columns;
		int realCol = indexRow % _columns;
		if (realRow < int(_terms.size()))
		{
			const Term &row = _terms.at(size_t(realRow));
			QString result = row.at(realCol);
			return result;
		}
		else
			Log::log()  << "ListModelPairsAssigned::data: row " << realRow << " out of range " << _terms.size() << std::endl;
	}
	else
		return ListModelAssignedInterface::data(index, role);

	return QVariant();
}

Terms* ListModelMultiTermsAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Terms* terms = new Terms();
	for (int index : indexes)
	{
		int realRow = index / _columns;
		int realCol = index % _columns;
		if (realRow < int(_terms.size()))
		{
			const Term &row = _terms.at(size_t(realRow));
			QString result = row.at(realCol);
			terms->add(Term(result));
		}
	}
	
	return terms;
}

void ListModelMultiTermsAssigned::removeTerms(const QList<int> &indexes)
{
	if (indexes.length() == 0) return;
	
	beginResetModel();

	QList<int> orderedIndexed = indexes;
	std::sort(orderedIndexed.begin(), orderedIndexed.end(), std::greater<int>());

	QList<QList<QString>> values = _terms.asQListOfQLists();
	for (const int &orderedIndexed: orderedIndexed)
	{
		int row = orderedIndexed / _columns;
		int col = orderedIndexed % _columns;
		
		if (row < values.length())
		{
			QList<QString> terms = values.at(row);
			bool isEmpty = true;
			for (int i = 0; i < _columns; i++)
			{
				if (i != col && !terms[i].isEmpty())
					isEmpty = false;
			}
			if (isEmpty)
				values.removeAt(row);
			else
			{
				terms[col] = QString();
				values.replace(row, terms);
			}
		}
	}
	
	_terms.set(values);

	endResetModel();

	emit modelChanged();
}

bool ListModelMultiTermsAssigned::canAddTerms(Terms *terms) const
{
	for (const Term &variable : *terms)
	{
		if ( ! isAllowed(variable))
			return false;
	}

	return true;
}

Terms* ListModelMultiTermsAssigned::addTerms(Terms *terms, int dropItemIndex, const QString&)
{
	beginResetModel();
	Terms* removedTerms = new Terms();
	
	if (terms->size() == 0)
		return removedTerms;
	
	QList<QList<QString>> values = _terms.asQListOfQLists();
	bool done = false;
	if (terms->size() == 1 && dropItemIndex >= 0)
	{
		int realRow = dropItemIndex / _columns;
		int realCol = dropItemIndex % _columns;
		if (realRow < values.size())
		{
			QList<QString> row = values[realRow];
			QString result = row[realCol];
			if (!result.isEmpty())
				removedTerms->add(Term(result));
			QString term = QString::fromStdString(terms[0].asString());
			row[realCol] = term;
			values[realRow] = row;
			done = true;
		}
	}
	
	if (!done)
	{
		QList<QString> newValues = terms->asQList();
		int index = 0;
		for (int row = 0; row < values.length() && index < newValues.length(); row++)
		{
			QList<QString> rowValues = values.at(row);
			bool changed = false;
			for (int col = 0; col < _columns && index < newValues.length(); col++)
			{
				if (rowValues[col].isEmpty())
				{
					rowValues[col] = newValues[index];
					changed = true;
					index++;
				}
			}
			if (changed)
				values.replace(row, rowValues);
		}
		
		while (index < newValues.length())
		{
			QList<QString> newTuple;
			for (int i = 0; i < _columns; i++)
			{
				if (index < newValues.length())
				{
					newTuple.push_back(newValues[index]);
					index++;
				}
				else
					newTuple.push_back(QString());
			}
			values.push_back(newTuple);
		}
	}
	
	_terms.set(values);
	endResetModel();

	emit modelChanged();

	return removedTerms;
}

void ListModelMultiTermsAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	if (indexes.length() != 1)
		return;
	
	bool isChanged = false;
	int fromIndex = indexes[0];
	if (fromIndex == dropItemIndex)
		return;
	
	int fromRow = fromIndex / _columns;
	int fromCol = fromIndex % _columns;
	
	beginResetModel();
	QList<QList<QString>> values = _terms.asQListOfQLists();
	if (fromRow < values.size())
	{
		QList<QString>& fromTuple = values[fromRow];
		QString fromValue = fromTuple[fromCol];
		if (!fromValue.isEmpty())
		{
			if (dropItemIndex >= 0)
			{
				int dropRow = dropItemIndex / _columns;
				int dropCol = dropItemIndex % _columns;
				if (dropRow < values.size())
				{
					QList<QString>& dropTuple = values[dropRow];
					QString dropValue = dropTuple[dropCol];
					fromTuple[fromCol] = dropValue;
					dropTuple[dropCol] = fromValue;
					isChanged = true;
				}
			}
			else
			{
				QList<QString>& dropTuple = values[values.size() - 1];
				for (int i = 0; i < _columns && !isChanged; i++)
				{
					if (dropTuple[i].isEmpty())
					{
						dropTuple[i] = fromValue;
						isChanged = true;
					}
				}

				if (!isChanged)
				{
					QList<QString> newRow {fromValue};
					for (int i = 1; i < _columns; i++)
						newRow.push_back(QString());
					values.push_back(newRow);
					isChanged = true;
				}
				fromTuple[fromCol] = QString();
			}
			
			bool removeFromTuple = true;
			for (int i = 0; i < _columns; i++)
			{
				if (!fromTuple[i].isEmpty())
					removeFromTuple = false;
			}
			if (removeFromTuple)
				values.removeAt(fromRow);
		}
	}

	_terms.set(values);	
	endResetModel();
	
	if (isChanged)
		emit modelChanged();
}
