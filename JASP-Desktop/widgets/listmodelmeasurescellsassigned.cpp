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
#include "boundqmllistviewmeasurescells.h"
#include "log.h"

using namespace std;


ListModelMeasuresCellsAssigned::ListModelMeasuresCellsAssigned(QMLListView* listView)
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
		_variables = variables.asQList();

	while (_variables.size() < _levels.size())
		_variables.push_back(QString());
	
	while (_variables.size() > _levels.size())
		_variables.pop_back();

	_terms.clear();
	int row = 0;
	for (const QString& level : _levels)
	{
		_terms.add(_variables.at(row), false);
		_terms.add(level, false);
		row++;
	}
	
	endResetModel();
}

void ListModelMeasuresCellsAssigned::sourceTermsChanged(const Terms *termsAdded, const Terms *termsRemoved)
{
	BoundQMLListViewMeasuresCells* measureCellsListView = dynamic_cast<BoundQMLListViewMeasuresCells*>(listView());
	if (measureCellsListView)
	{
		initLevels(measureCellsListView->getLevels());
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
		int realIndex = index / 2;
		if (realIndex < _variables.size())
			terms.add(Term(_variables[realIndex]));
	}
	
	return terms;
}

Terms ListModelMeasuresCellsAssigned::addTerms(const Terms& terms, int dropItemIndex, const QString&)
{
	beginResetModel();
	if (dropItemIndex >= 0)
		dropItemIndex = dropItemIndex / 2;
	Terms termsToSendBack;
	if (dropItemIndex >= 0)
	{
		if (terms.size() > 1 || dropItemIndex >= _variables.size())
			termsToSendBack.set(terms);
		else
		{
			const Term& newTerm = terms.at(0);
			const QString& oldTerm = _variables.at(dropItemIndex);
			if (!oldTerm.isEmpty())
				termsToSendBack.add(Term(oldTerm));
			_variables.replace(dropItemIndex, newTerm.asQString());
			_terms.replace(dropItemIndex * 2, newTerm);
		}
	}
	else
	{
		uint index = 0;
		for (int i = 0; i < _variables.size() && index < terms.size(); i++)
		{
			const QString& oldTerm = _variables.at(i);
			if (oldTerm.isEmpty())
			{
				const Term& newTerm = terms.at(index);
				_variables.replace(i, newTerm.asQString());
				_terms.replace(2 * i, newTerm);
				index++;
			}
		}
		
		for (uint i = index; i < terms.size(); i++)
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
	
	int fromIndex = indexes[0];
	int fromRow = fromIndex / 2;
	if (fromRow < 0 || fromRow >= _variables.size())
		return;
	int dropRow = dropItemIndex / 2;
	if (dropRow >= _variables.size())
		return;
	
	beginResetModel();
	QString fromValue = _variables.at(fromRow);
	QString dropValue = _variables.at(dropRow);
	_variables.replace(fromRow, dropValue);
	_terms.replace(fromRow * 2, dropValue);
	_variables.replace(dropRow, fromValue);
	_terms.replace(dropRow * 2, fromValue);
	endResetModel();
	
	emit modelChanged();
}

void ListModelMeasuresCellsAssigned::removeTerms(const QList<int> &indexes)
{
	beginResetModel();
	for (int i = 0; i < indexes.length(); i++)
	{
		int index = indexes[i] / 2;
		if (index < _variables.size())
		{
			_variables.replace(index, QString());
			_terms.replace(index * 2, QString());
		}
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

	if (role == ListModel::SelectableRole)
	{
		int indexRow = index.row();
		int realCol = indexRow % 2;
		return realCol == 0 && !_terms.at(size_t(indexRow)).asString().empty();
	}
	if (role == ListModel::TypeRole)
	{
		int indexRow = index.row();
		int realCol = indexRow % 2;
		if (realCol == 0)
			return "variable";
		else
			return "level";
	}
	else
		return ListModelAssignedInterface::data(index, role);
}
