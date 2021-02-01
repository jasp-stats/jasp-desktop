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
		_setTerms(variables, false);

	_fitTermsWithLevels();

	if (availableModel() != nullptr)
		availableModel()->removeTermsInAssignedList();
	
	endResetModel();
}

void ListModelMeasuresCellsAssigned::_fitTermsWithLevels()
{
	while (terms().size() < size_t(_levels.size()))
		_addTerm(QString(), false);

	if (terms().size() > size_t(_levels.size()))
	{
		std::vector<Term> newTerms = terms().terms();
		while (newTerms.size() > size_t(_levels.size()))
			newTerms.pop_back();
		_setTerms(newTerms, false);
	}
}

void ListModelMeasuresCellsAssigned::sourceTermsReset()
{
	VariablesListBase* measureCellsListView = dynamic_cast<VariablesListBase*>(listView());
	if (measureCellsListView)
	{
		BoundControlMeasuresCells* boundControl = dynamic_cast<BoundControlMeasuresCells*>(measureCellsListView->boundControl());
		initLevels(boundControl->getLevels());
		availableModel()->removeTermsInAssignedList();
	}
	else
		Log::log() << "ListView from Measures cells model is not of a Measures Cell type!!";
}

Terms ListModelMeasuresCellsAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Terms result;
	for (int index : indexes)
	{
		size_t realIndex = size_t(index / 2);
		if (realIndex < terms().size())
			result.add(terms().at(realIndex));
	}
	
	return result;
}

void ListModelMeasuresCellsAssigned::initTerms(const Terms &terms, const ListModel::RowControlsValues &allValuesMap)
{
	ListModelAssignedInterface::initTerms(terms, allValuesMap);
	_fitTermsWithLevels();
}

Terms ListModelMeasuresCellsAssigned::addTerms(const Terms& termsToAdd, int dropItemIndex, JASPControl::AssignType)
{
	beginResetModel();
	if (dropItemIndex >= 0)
		dropItemIndex = dropItemIndex / 2;
	Terms termsToSendBack;
	if (dropItemIndex >= 0)
	{
		if (termsToAdd.size() > 1 || dropItemIndex >= int(terms().size()))
			termsToSendBack.set(termsToAdd);
		else
		{
			const Term& newTerm = termsToAdd.at(0);
			const Term& oldTerm = terms().at(size_t(dropItemIndex));
			if (!oldTerm.asString().empty())
				termsToSendBack.add(Term(oldTerm));
			_replaceTerm(dropItemIndex, newTerm);
		}
	}
	else
	{
		size_t index = 0;
		for (size_t i = 0; i < terms().size() && index < termsToAdd.size(); i++)
		{
			const Term& oldTerm = terms().at(i);
			if (oldTerm.asQString().isEmpty())
			{
				const Term& newTerm = termsToAdd.at(index);
				_replaceTerm(int(i), newTerm);
				index++;
			}
		}
		
		for (size_t i = index; i < termsToAdd.size(); i++)
		{
			const Term& term = termsToAdd.at(i);
			termsToSendBack.add(term);
		}
	}
	
	endResetModel();
	
	return termsToSendBack;
}

void ListModelMeasuresCellsAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	if (indexes.length() != 1 || dropItemIndex < 0)
		return;
	
	size_t fromIndex = size_t(indexes[0]);
	size_t fromRow = fromIndex / 2;
	if (fromRow >= terms().size())
		return;
	size_t dropRow = size_t(dropItemIndex / 2);
	if (dropRow >= terms().size())
		return;
	
	beginResetModel();
	Term fromValue = terms().at(fromRow);
	Term dropValue = terms().at(dropRow);
	_replaceTerm(int(fromRow), dropValue);
	_replaceTerm(int(dropRow), fromValue);
	endResetModel();
}

void ListModelMeasuresCellsAssigned::removeTerms(const QList<int> &indexes)
{
	beginResetModel();
	for (int i = 0; i < indexes.length(); i++)
	{
		int index = indexes[i] / 2;
		if (index < int(terms().size()))
			_replaceTerm(index, QString());
	}
	endResetModel();	
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
			return terms()[size_t(realRow)].asQString();
		else
			return _levels[realRow];
	}
	else if (role == ListModel::SelectableRole)
	{
		return realCol == 0 && !terms().at(size_t(realRow)).asString().empty();
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
