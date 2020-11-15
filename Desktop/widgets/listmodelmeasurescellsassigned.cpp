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

#include "listmodelmeasurescellsassigned.h"
#include "listmodelrepeatedmeasuresfactors.h"
#include "variableslistbase.h"
#include "boundcontrolmeasurescells.h"
#include "log.h"

using namespace std;


ListModelMeasuresCellsAssigned::ListModelMeasuresCellsAssigned(JASPListControl* listView)
	: ListModelAssignedInterface(listView)
{
}

void ListModelMeasuresCellsAssigned::initLevels(const Terms &levels, const Terms &variables, bool initVariables)
{
	beginResetModel();
	_levels.clear();
	vector<vector<string> > allLevels = levels.asVectorOfVectors();

	for (const vector<string>& levels : allLevels)
	{
		string concatLevels;
		if (levels.size() > 0)
			concatLevels = levels[0];
		for (uint i = 1; i < levels.size(); i++)
			concatLevels += "," + levels[i];
		_levels.push_back(QString::fromStdString(concatLevels));
	}
	
	if (initVariables)
		_terms = variables;

	_fitTermsWithLevels();

	if (source() != nullptr)
		source()->removeTermsInAssignedList();
	
	endResetModel();
}

void ListModelMeasuresCellsAssigned::_fitTermsWithLevels()
{
	while (_terms.size() < size_t(_levels.size()))
		_terms.add(QString(), false);

	if (_terms.size() > size_t(_levels.size()))
	{
		std::vector<Term> terms = _terms.terms();
		while (terms.size() > size_t(_levels.size()))
			terms.pop_back();
		_terms.set(terms);
	}
}

void ListModelMeasuresCellsAssigned::sourceTermsChanged(const Terms *termsAdded, const Terms *termsRemoved)
{
	VariablesListBase* measureCellsListView = dynamic_cast<VariablesListBase*>(listView());
	if (measureCellsListView)
	{
		BoundControlMeasuresCells* boundControl = dynamic_cast<BoundControlMeasuresCells*>(measureCellsListView->boundControl());
		initLevels(boundControl->getLevels());
		source()->removeTermsInAssignedList();
		emit modelChanged(termsAdded, termsRemoved);
	}
	else
		Log::log() << "ListView from Measures cells model is not of a Measures Cell type!!";
}

Terms ListModelMeasuresCellsAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Terms terms;
	for (int index : indexes)
	{
		size_t realIndex = size_t(index / 2);
		if (realIndex < _terms.size())
			terms.add(_terms[realIndex]);
	}
	
	return terms;
}

void ListModelMeasuresCellsAssigned::initTerms(const Terms &terms, const ListModel::RowControlsOptions &allOptionsMap)
{
	ListModelAssignedInterface::initTerms(terms, allOptionsMap);
	_fitTermsWithLevels();
}

Terms ListModelMeasuresCellsAssigned::addTerms(const Terms& terms, int dropItemIndex, JASPControl::AssignType)
{
	beginResetModel();
	if (dropItemIndex >= 0)
		dropItemIndex = dropItemIndex / 2;
	Terms termsToSendBack;
	if (dropItemIndex >= 0)
	{
		if (terms.size() > 1 || dropItemIndex >= int(_terms.size()))
			termsToSendBack.set(terms);
		else
		{
			const Term& newTerm = terms.at(0);
			const Term& oldTerm = _terms.at(size_t(dropItemIndex));
			if (!oldTerm.asString().empty())
				termsToSendBack.add(Term(oldTerm));
			_terms.replace(dropItemIndex, newTerm);
		}
	}
	else
	{
		size_t index = 0;
		for (size_t i = 0; i < _terms.size() && index < terms.size(); i++)
		{
			const Term& oldTerm = _terms.at(i);
			if (oldTerm.asQString().isEmpty())
			{
				const Term& newTerm = terms.at(index);
				_terms.replace(int(i), newTerm);
				index++;
			}
		}
		
		for (size_t i = index; i < terms.size(); i++)
		{
			const Term& term = terms.at(i);
			termsToSendBack.add(term);
		}
	}
	
	endResetModel();
	
	emit modelChanged();
	
	return termsToSendBack;
}

void ListModelMeasuresCellsAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	if (indexes.length() != 1 || dropItemIndex < 0)
		return;
	
	size_t fromIndex = size_t(indexes[0]);
	size_t fromRow = fromIndex / 2;
	if (fromRow >= _terms.size())
		return;
	size_t dropRow = size_t(dropItemIndex / 2);
	if (dropRow >= _terms.size())
		return;
	
	beginResetModel();
	Term fromValue = _terms.at(fromRow);
	Term dropValue = _terms.at(dropRow);
	_terms.replace(int(fromRow), dropValue);
	_terms.replace(int(dropRow), fromValue);
	endResetModel();
	
	emit modelChanged();
}

void ListModelMeasuresCellsAssigned::removeTerms(const QList<int> &indexes)
{
	beginResetModel();
	for (int i = 0; i < indexes.length(); i++)
	{
		int index = indexes[i] / 2;
		if (index < int(_terms.size()))
			_terms.replace(index, QString());
	}
	endResetModel();
	
	emit modelChanged();
}

QVariant ListModelMeasuresCellsAssigned::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
	{
		Log::log()  << "ListModelMeasuresCellsAssigned::data: Data invalid!" << std::endl;
		return QVariant();
	}

	int indexRow = index.row();
	int realCol = indexRow % 2;
	int realRow = indexRow / 2;

	if (realRow >= _levels.size())
		return QVariant();

	if (role == Qt::DisplayRole || role == ListModel::NameRole)
	{
		if (realCol == 0)
			return _terms[size_t(realRow)].asQString();
		else
			return _levels[realRow];
	}
	else if (role == ListModel::SelectableRole)
	{
		return realCol == 0 && !_terms.at(size_t(realRow)).asString().empty();
	}
	else if (role == ListModel::SelectedRole)
	{
		if (_selectedItems.contains(indexRow) && realCol == 0)
			return true;
		else
			return false;
	}
	else if (role == ListModel::TypeRole)
	{
		if (realCol == 0)
			return "variable";
		else
			return "level";
	}
	else
		return ListModelAssignedInterface::data(index, role);
}
