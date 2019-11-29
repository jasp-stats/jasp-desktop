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

void ListModelMultiTermsAssigned::removeTerms(const QList<int> &indexes)
{
	if (indexes.length() == 0) return;
	
	beginResetModel();

	QList<int> orderedIndexed = indexes;
	std::sort(orderedIndexed.begin(), orderedIndexed.end(), std::greater<int>());

	for (const int &orderedIndexed: orderedIndexed)
	{
		int row = orderedIndexed / _columns;
		int col = orderedIndexed % _columns;
		
		if (row < _tuples.length())
		{
			const Terms& terms = _tuples.at(row);
			bool isEmpty = true;
			for (int i = 0; i < _columns; i++)
			{
				if (i != col && !terms.at(size_t(i)).asQString().isEmpty())
					isEmpty = false;
			}
			if (isEmpty)
				_tuples.removeAt(row);
			else
			{
				Terms newTerms = terms;
				newTerms.replace(col, QString());
				_tuples[row] = newTerms;
			}
		}
	}
	
	_setTerms();
	endResetModel();

	emit modelChanged();
}

void ListModelMultiTermsAssigned::_setTerms()
{
	_terms.clear();
	for (const Terms& terms : _tuples)
	{
		for (const Term& term : terms)
			_terms.add(term, false);
	}
}


Terms* ListModelMultiTermsAssigned::addTerms(Terms *termsToAdd, int dropItemIndex, const QString&)
{
	beginResetModel();
	Terms* removedTerms = new Terms();
	
	if (termsToAdd->size() == 0)
		return removedTerms;
	
	bool done = false;
	if (termsToAdd->size() == 1 && dropItemIndex >= 0)
	{
		int realRow = dropItemIndex / _columns;
		int realCol = dropItemIndex % _columns;
		if (realRow < _tuples.size())
		{
			Terms row = _tuples[realRow];
			const Term& term = row[size_t(realCol)];
			if (!term.asQString().isEmpty())
				removedTerms->add(term);
			row.replace(realCol, termsToAdd->at(0));
			_tuples[realRow] = row;
			done = true;
		}
	}
	
	if (!done)
	{
		size_t index = 0;
		for (int row = 0; row < _tuples.length() && index < termsToAdd->size(); row++)
		{
			Terms tuple = _tuples.at(row);
			bool changed = false;
			for (int col = 0; col < _columns && index < termsToAdd->size(); col++)
			{
				if (tuple[size_t(col)].asQString().isEmpty())
				{
					tuple.replace(col, termsToAdd->at(index));
					changed = true;
					index++;
				}
			}
			if (changed)
				_tuples[row] = tuple;
		}
		
		while (index < termsToAdd->size())
		{
			Terms newTuple;
			for (int i = 0; i < _columns; i++)
			{
				if (index < termsToAdd->size())
				{
					newTuple.add(termsToAdd->at(index), false);
					index++;
				}
				else
					newTuple.add(QString(), false);
			}
			_tuples.push_back(newTuple);
		}
	}
	
	_setTerms();
	endResetModel();

	emit modelChanged();

	return removedTerms;
}

void ListModelMultiTermsAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	if (indexes.length() != 1)
		return;
	
	int fromIndex = indexes[0];
	if (fromIndex == dropItemIndex)
		return;
	
	int fromRow = fromIndex / _columns;
	int fromCol = fromIndex % _columns;

	if (fromRow >= _tuples.size())
		return;

	Terms fromTuple = _tuples[fromRow];
	Terms dropTuple;
	int dropRow = -1;
	bool isChanged = false;
	Term fromValue = fromTuple[size_t(fromCol)];

	if (fromValue.asQString().isEmpty())
		return;

	beginResetModel();

	if (dropItemIndex >= 0)
	{
		dropRow = dropItemIndex / _columns;
		int dropCol = dropItemIndex % _columns;
		if (dropRow < _tuples.size())
		{
			dropTuple = _tuples[dropRow];
			Term dropValue = dropTuple[size_t(dropCol)];
			dropTuple.replace(dropCol, fromValue);
			if (dropRow == fromRow)
			{
				dropTuple.replace(fromCol, dropValue);
				fromTuple = dropTuple;
			}
			else
				fromTuple.replace(fromCol, dropValue);
			isChanged = true;
		}
	}
	else
	{
		fromTuple.replace(fromCol, QString());
		dropRow = _tuples.size() - 1;
		dropTuple = _tuples[dropRow];
		for (int i = 0; i < _columns && !isChanged; i++)
		{
			if (dropTuple[size_t(i)].asQString().isEmpty())
			{
				dropTuple.replace(i, fromValue);
				isChanged = true;
			}
		}
	}

	_tuples[fromRow] = fromTuple;

	if (isChanged)
		_tuples[dropRow] = dropTuple;
	else
	{
		Terms newRow;
		newRow.add(fromValue);
		for (int i = 1; i < _columns; i++)
			newRow.add(QString(), false);
		_tuples.push_back(newRow);
	}

	bool removeFromTuple = true;
	for (int i = 0; i < _columns; i++)
	{
		if (!fromTuple[size_t(i)].asQString().isEmpty())
			removeFromTuple = false;
	}
	if (removeFromTuple)
		_tuples.removeAt(fromRow);

	_setTerms();
	endResetModel();
	
	emit modelChanged();
}
