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

#include "listmodeldraggable.h"
#include "analysisform.h"
#include "controls/jasplistcontrol.h"

ListModelDraggable::ListModelDraggable(JASPListControl* listView)
	: ListModel(listView)
{
}

ListModelDraggable::~ListModelDraggable()
{
}

Terms ListModelDraggable::termsFromIndexes(const QList<int> &indexes) const
{
	Terms result;

	for (int index : indexes)
		if (size_t(index) < terms().size())
			result.add(terms().at(index));
	
	return result;
}

QList<int> ListModelDraggable::indexesFromTerms(const  Terms & termsIn) const
{
	std::set<int>		result;
	std::map<Term, int> termToIndex;
	
	for(size_t t=0; t<terms().size(); t++)
		termToIndex[terms().at(t)] = t;

	for(const Term & term : termsIn)
		if(termToIndex.count(term))
			result.insert(termToIndex[term]);
	
	return tql(result);
}

void ListModelDraggable::removeTerms(const QList<int> &indices)
{
	if(!indices.count())
		return;
	
	beginResetModel();

	Terms termsToRemove;

	for (int index : indices)
		if (index < rowCount())
			termsToRemove.add(terms().at(size_t(index)));

	_removeTerms(termsToRemove);

	endResetModel();
}


void ListModelDraggable::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	if(!indexes.count())
		return;

	JASPControl::DropMode _dropMode = dropMode();
	if (indexes.length() == 0 || _dropMode == JASPControl::DropMode::DropNone)
		return;	

	beginResetModel();
	Terms terms = termsFromIndexes(indexes);
	removeTerms(indexes); // Remove first before adding: we cannot add terms that already exist
	for (int index : indexes)
		if (index < dropItemIndex)
			dropItemIndex--;

	Terms removedTerms = addTerms(terms, dropItemIndex);
	if (removedTerms.size() > 0)
		addTerms(removedTerms);
	
	endResetModel();
}

Terms ListModelDraggable::addTerms(const Terms& terms, int dropItemIndex, const RowControlsValues&)
{
	if (terms.size() > 0)
	{
		beginResetModel();
		_addTerms(terms);
		endResetModel();
	}

	return Terms();
}

Terms ListModelDraggable::canAddTerms(const Terms& terms) const
{
	Terms result;
	for (const Term &term : terms)
	{
		if (isAllowed(term))
			result.add(term);
	}

	return result;
}

bool ListModelDraggable::isAllowed(const Term &term) const
{
	if (!listView()->allowAnalysisOwnComputedColumns())
	{
		if (listView()->form()->isOwnComputedColumn(term.asString()))
			return false;
	}

	const QSet<columnType>& variableTypesAllowed = listView()->variableTypesAllowed();

	if (variableTypesAllowed.empty() || term.size() > 1)
		return true;
	
	columnType	variableType = (columnType)requestInfo(VariableInfo::VariableType, term.asQString()).toInt();

	return variableTypesAllowed.contains(variableType);
}
