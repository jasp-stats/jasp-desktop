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
#include "controls/jasplistcontrol.h"
#include "log.h"


using namespace std;

ListModelMultiTermsAssigned::ListModelMultiTermsAssigned(JASPListControl* listView, int columns)
	: ListModelAssignedInterface(listView)
	, _columns(columns)
{
	_allowDuplicatesInMultipleColumns = listView->property("allowDuplicatesInMultipleColumns").toBool();
}

void ListModelMultiTermsAssigned::initTerms(const Terms &terms, const Terms::RelatedValuesPerTerm& allValuesMap)
{
	beginResetModel();

	// The terms are sent either in groups (terms has multiple components) or one by one (no multiple components)
	if (terms.size() > 0)
	{
		if (terms[0].components().size() > 1)
		{
			_tuples.clear();

			for (const Term& term : terms)
			{
				Terms row;

				for (const QString& comp : term.components())
					row.add(comp, false);
				_tuples.push_back(row);
			}
		}
		else
		{
			// In this case discard elements in tuples that are not in terms.
			// And then add the terms that were not in the tuples
			QList<Terms> newTuples;
			for (int i = 0; i < _tuples.size(); i++)
			{
				Terms row = _tuples[i];
				row.discardWhatIsntTheseTerms(terms);	//name-based set partition (mds-plan post): the option value is authoritative for what is bound, stamps take no part here
				if (row.size() > 0)
				{
					for (int j = int(row.size()); j < _columns; j++)
						row.add(QString(), false);
					newTuples.push_back(row);
				}
			}

			_tuples = newTuples;
			Terms unusedTerms = terms;

			for (const Terms& tuple : _tuples)
				unusedTerms.discardWhatDoesContainTheseTerms(tuple);

			size_t index = 0;
			while (unusedTerms.size() > index)
			{
				Terms row;
				for (int i = 0; i < _columns; i++)
				{
					if (unusedTerms.size() > index)
						row.add(unusedTerms.at(index), false);
					else
						row.add(QString(), false);
					index++;
				}
				_tuples.push_back(row);
			}
		}
	}

	_setTerms();

	_rowControlsValues = allValuesMap;
	endResetModel();

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
				if (i != col && !terms.at(size_t(i)).value().isEmpty())
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
}

Terms ListModelMultiTermsAssigned::reconcileProviderChange(int filterIdToUpdate, const Terms &newUniverse)
{
	//mds-plan post (ORIGIN-AUTHORITATIVE): the tuple version of the reconcile - the tuples are
	//the source of truth, the flattened _terms is rebuilt from them afterwards. Participation is
	//exactly the base rule (termFollowsProviderChange): a cell stamped `filterIdToUpdate`
	//follows, -1 and foreign stamps never move. Survivors are REPLACED BY the delivered
	//universe term and the row maps re-key like the rename flow does; vanished cells are
	//returned for the handler to clear.
	Terms vanished;
	bool changed = false;

	for (Terms& tuple : _tuples)
	{
		for (Term& term : tuple)
		{
			if (term.value().isEmpty() || !termFollowsProviderChange(term.filterId(), filterIdToUpdate))
				continue;

			const TermRowKey oldKey = TermRowKey::ofTerm(term);

			if (newUniverse.containsValue(term.value()))	//NAME-ONLY on purpose (mds-plan post)
			{
				term = newUniverse.at(size_t(newUniverse.indexOfValue(term.value())));

				const TermRowKey newKey = TermRowKey::ofTerm(term);
				if (newKey != oldKey)
				{
					if (_rowControlsMap.contains(oldKey))
						_rowControlsMap.insert(newKey, _rowControlsMap.take(oldKey));
					if (_rowControlsValues.contains(oldKey))
						_rowControlsValues.insert(newKey, _rowControlsValues.take(oldKey));
				}
			}
			else
				vanished.add(term);

			changed = true;
		}
	}

	if (changed)
		_setTerms();	//rebuilds the flattened _terms (and row controls) from the reconciled tuples

	return vanished;
}

void ListModelMultiTermsAssigned::availableTermsResetHandler(Terms termsAdded, Terms termsRemoved, int filterIdToUpdate)
{
	//mds-plan post (ORIGIN-AUTHORITATIVE): the pool's provider moved on: tuple cells stamped
	//`filterIdToUpdate` are replaced by the delivered universe terms when the column survives
	//and die when it vanished; -1 and foreign stamps never move. The vanished cells (plus the
	//pool's own removals) are cleared through the usual cell machinery, exactly like a plain
	//column removal.
	Terms vanished;
	if (availableModel())
		vanished = reconcileProviderChange(filterIdToUpdate, availableModel()->allTerms());

	if (termsRemoved.size() == 0 && vanished.size() == 0)
		return;

	QList<int> indexes;
	int i = 0;
	for (const Term& oneTerm : terms())
	{
		if (termsRemoved.containsValue(oneTerm) || vanished.containsValue(oneTerm))		indexes.append(i);
		i++;
	}

	if (indexes.size() > 0)
		removeTerms(indexes);
}

void ListModelMultiTermsAssigned::sourceVariableNamesChanged(QMap<QString, QString> map)
{
	// Update first _tuples, then call the parent to handle the variables names changed
	for (Terms& terms : _tuples)
	{
		for (const Term& term : terms)
			if (map.contains(term.label()))
				terms.replaceVariableName(fq(term.label()), fq(map[term.label()]));
	}

	ListModelAssignedInterface::sourceVariableNamesChanged(map);
}

void ListModelMultiTermsAssigned::_setTerms()
{
	Terms newTerms;
	for (const Terms& terms : _tuples)
	{
		for (const Term& term : terms)
			newTerms.add(term, false);
	}

	ListModel::_setTerms(newTerms);
}

Terms ListModelMultiTermsAssigned::canAddTerms(const Terms& terms) const
{
	if (listView()->maxRows() >= 0 && int(terms.size()) > (listView()->maxRows() * 2))
		return Terms();

	return ListModelDraggable::canAddTerms(terms);
}


Terms ListModelMultiTermsAssigned::addTerms(const Terms& termsToAdd, int dropItemIndex, const Terms::RelatedValuesPerTerm&)
{
	Terms termsToReturn;

	if (termsToAdd.size() == 0)
		return termsToReturn;

	beginResetModel();
	bool done = false;
	if (termsToAdd.size() == 1 && dropItemIndex >= 0)
	{
		// Case when only 1 term is added on one particular place
		// . Check first if it is possible to the term at this place.
		// . Then set the term at this place.
		// . It there was already a term at that place, return it.
		int realRow = dropItemIndex / _columns;
		int realCol = dropItemIndex % _columns;
		if (realRow < _tuples.size())
		{
			Terms row = _tuples[realRow];
			const Term& termToAdd = termsToAdd.at(0);

			if (row.containsValue(termToAdd) && !_allowDuplicatesInMultipleColumns)
				termsToReturn.add(termToAdd);
			else
			{
				const Term& term = row[size_t(realCol)];
				if (!term.value().isEmpty())
					termsToReturn.add(term);
				row.replace(realCol, termToAdd);
				_tuples[realRow] = row;
			}
			done = true;
		}
	}

	if (!done)
	{
		// First try to set the terms to the empty places
		size_t index = 0;
		for (int row = 0; row < _tuples.length() && index < termsToAdd.size(); row++)
		{
			Terms tuple = _tuples.at(row);
			bool changed = false;
			for (int col = 0; col < _columns && index < termsToAdd.size(); col++)
			{
				if (tuple[size_t(col)].value().isEmpty())
				{
					const Term& termToAdd = termsToAdd.at(index);
					if (tuple.containsValue(termToAdd) && !_allowDuplicatesInMultipleColumns)
						termsToReturn.add(termToAdd);
					else
					{
						tuple.replace(col, termToAdd);
						changed = true;
					}
					index++;
				}
			}
			if (changed)
				_tuples[row] = tuple;
		}

		// If there still some terms to add, add them at the end of the list
		while (index < termsToAdd.size() && (listView()->maxRows() < 0 || _tuples.size() < listView()->maxRows()))
		{
			Terms newTuple;
			for (int i = 0; i < _columns; i++)
			{
				if (index < termsToAdd.size())
				{
					newTuple.add(termsToAdd.at(index), false);
					index++;
				}
				else
					newTuple.add(QString(), false);
			}
			_tuples.push_back(newTuple);
		}

		for (; index < termsToAdd.size(); index++)
			termsToReturn.add(termsToAdd.at(index));
	}

	_setTerms();
	endResetModel();

	return termsToReturn;
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
	bool addNewRow = false;
	Term fromValue = fromTuple[size_t(fromCol)];

	if (fromValue.value().isEmpty())
		return;

	beginResetModel();

	if (dropItemIndex >= 0)
	{
		// First handle the case when the term is dropped on one particular place
		dropRow = dropItemIndex / _columns;
		int dropCol = dropItemIndex % _columns;

		if (dropRow < _tuples.size())
		{
			dropTuple = _tuples[dropRow];
			Term dropValue = dropTuple[size_t(dropCol)];

			if (dropRow == fromRow)
			{
				if (fromCol != dropCol)
				{
					dropTuple.replace(dropCol, fromValue);
					dropTuple.replace(fromCol, dropValue);
					_tuples[dropRow] = dropTuple;
				}
			}
			else
			{
				// If it does not allow duplicates, and the dropTuple contains the fromValue or the fromTuple contains the dropValue (if not empty), then do not exchange the values.
				if (!(!_allowDuplicatesInMultipleColumns && (dropTuple.containsValue(fromValue) || (!dropValue.value().isEmpty() && fromTuple.containsValue(dropValue)))))
				{
					dropTuple.replace(dropCol, fromValue);
					fromTuple.replace(fromCol, dropValue);
					_tuples[dropRow] = dropTuple;
					_tuples[fromRow] = fromTuple;
				}
			}
		}
		else
		{
			fromTuple.replace(fromCol, QString());
			_tuples[fromRow] = fromTuple;
			addNewRow = true;
		}
	}
	else
	{
		// The term is dropped at the end of the list.
		// Check whether the last row has still some place
		fromTuple.replace(fromCol, QString());
		_tuples[fromRow] = fromTuple;

		dropRow = _tuples.size() - 1;
		dropTuple = _tuples[dropRow];
		addNewRow = true;

		// It it does not allow duplicates, and the last row contains the fromValue, then do not try to add the fromValue to the last row
		if (!(!_allowDuplicatesInMultipleColumns && dropTuple.containsValue(fromValue)))
		{
			for (int i = 0; i < _columns && !addNewRow; i++)
			{
				if (dropTuple[size_t(i)].value().isEmpty())
				{
					dropTuple.replace(i, fromValue);
					_tuples[dropRow] = dropTuple;
					addNewRow = false;
				}
			}
		}
	}

	if (addNewRow)
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
		if (!fromTuple[size_t(i)].value().isEmpty())
			removeFromTuple = false;
	}
	if (removeFromTuple)
		_tuples.removeAt(fromRow);

	_setTerms();
	endResetModel();
}
