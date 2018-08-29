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

#include "listmodelmeasurescells.h"
#include <QDebug>

using namespace std;


ListModelMeasuresCells::ListModelMeasuresCells(AnalysisQMLForm *form, QQuickItem* item)
	: ListModelTermsAssignedInterface(form, item)
{
	_source = NULL;
}

void ListModelMeasuresCells::initLevels(const Terms &terms)
{
	beginResetModel();
	_levels.clear();
	
	vector<vector<string> > allLevels = terms.asVectorOfVectors();
	
	for (const vector<string>& levels : allLevels)
	{
		string concatLevels;
		if (levels.size() > 0)
			concatLevels = levels[0];
		for (uint i = 1; i < levels.size(); i++)
			concatLevels += "," + levels[i];
		_levels.push_back(QString::fromStdString(concatLevels));
	}
	
	while (_variables.size() < _levels.size())
		_variables.push_back(QString());
	
	while (_variables.size() > _levels.size())
		_variables.pop_back();
	
	endResetModel();
}

void ListModelMeasuresCells::initVariables(const Terms &terms)
{
	
}

Terms *ListModelMeasuresCells::termsFromIndexes(const QList<int> &indexes) const
{
	Terms* terms = new Terms;
	for (const int &index : indexes)
	{
		int realIndex = index / 2;
		if (realIndex < _variables.size())
			terms->add(Term(_variables[realIndex]));
	}
	
	return terms;
}

Terms *ListModelMeasuresCells::addTerms(Terms *terms, int dropItemIndex)
{
	beginResetModel();
	if (dropItemIndex >= 0)
		dropItemIndex = dropItemIndex / 2;
	Terms* termsToSendBack = new Terms();
	if (dropItemIndex >= 0)
	{
		if (terms->size() > 1 || dropItemIndex >= _variables.size())
			termsToSendBack->set(*terms);
		else
		{
			const Term& newTerm = terms->at(0);
			const QString& oldTerm = _variables.at(dropItemIndex);
			if (!oldTerm.isEmpty())
				termsToSendBack->add(Term(oldTerm));
			_variables.replace(dropItemIndex, newTerm.asQString());
		}
	}
	else
	{
		uint index = 0;
		for (int i = 0; i < _variables.size() && index < terms->size(); i++)
		{
			const QString& oldTerm = _variables.at(i);
			if (oldTerm.isEmpty())
			{
				const Term& newTerm = terms->at(index);
				_variables.replace(i, newTerm.asQString());
				index++;
			}
		}
		
		for (uint i = index; i < terms->size(); i++)
		{
			const Term& term = terms->at(i);
			termsToSendBack->add(term);
		}
	}
	
	endResetModel();
	
	emit termsChanged();
	
	return termsToSendBack;
}

void ListModelMeasuresCells::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	if (indexes.length() != 1 || dropItemIndex < 0)
		return;
	
	int fromIndex = indexes[0];
	int fromCol = fromIndex / 2;
	if (fromCol < 0 || fromCol >= _variables.size())
		return;
	int dropCol = dropItemIndex / 2;
	if (dropCol >= _variables.size())
		return;
	
	beginResetModel();
	QString fromValue = _variables.at(fromCol);
	_variables.replace(fromCol, _variables.at(dropCol));
	_variables.replace(dropCol, fromValue);
	endResetModel();
	
	emit termsChanged();
}

void ListModelMeasuresCells::removeTerms(const QList<int> &indexes)
{
	beginResetModel();
	for (int i = 0; i < indexes.length(); i++)
	{
		int index = indexes[i] / 2;
		if (index < _variables.size())
			_variables.replace(index, QString());
	}
	endResetModel();
	
	emit termsChanged();
}

int ListModelMeasuresCells::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	return _levels.size() * 2;
}

QVariant ListModelMeasuresCells::data(const QModelIndex &index, int role) const
{
	if ( ! index.isValid())
	{
		qDebug() << "Data invalid!";
		return QVariant();
	}

	QString result;
	int indexRow = index.row();
	int realRow = indexRow / 2;
	int realCol = indexRow % 2;
	
	if (role == Qt::DisplayRole || role == ListModelDraggableTerms::NameRole)
	{
		if (realCol < _levels.size())
		{
			if (realCol == 0)
				result = _variables[realRow];
			else
				result = _levels[realRow];
		}
	}
	else if (role == ListModelDraggableTerms::TypeRole)
	{
		if (realCol == 0)
			result = "variable";
		else
			result = "level";
	}
	else
	{
		qDebug() << "Unused Role: " << role;
	}

	return result;
}
