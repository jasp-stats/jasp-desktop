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

ListModelPairsAssigned::ListModelPairsAssigned(QMLListView* listView)
	: ListModelAssignedInterface(listView)
{
	_copyTermsWhenDropped = true;
}

int ListModelPairsAssigned::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	int size = _terms.size();
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
		uint realRow = indexRow / 2;
		uint realCol = indexRow % 2;
		if (realRow < _terms.size())
		{
			const Term &row = _terms.at(realRow);
			QString result = row.at(realCol);
			return result;
		}
		else
			qDebug() << "ListModelPairsAssigned: row " << realRow << " out of range " << _terms.size();
	}
	else
	{
		qDebug() << "Unused Role: " << role;
	}

	return QVariant();
}

Terms* ListModelPairsAssigned::termsFromIndexes(const QList<int> &indexes) const
{
	Terms* terms = new Terms();
	for (int index : indexes)
	{
		uint realRow = index / 2;
		uint realCol = index % 2;
		if (realRow < _terms.size())
		{
			const Term &row = _terms.at(realRow);
			QString result = row.at(realCol);
			terms->add(Term(result));
		}
	}
	
	return terms;
}

void ListModelPairsAssigned::removeTerms(const QList<int> &indexes)
{
	if (indexes.length() == 0) return;
	
	beginResetModel();

	QList<int> orderedIndexed = indexes;
	std::sort(orderedIndexed.begin(), orderedIndexed.end(), qGreater<int>());

	QList<QList<QString>> values = _terms.asQListOfQLists();
	for (const int &orderedIndexed: orderedIndexed)
	{
		int row = orderedIndexed / 2;
		int col = orderedIndexed % 2;
		
		if (row < values.length())
		{
			const QList<QString>& pairTerm = values.at(row);
			const QString& relatedTerm = pairTerm.at(1-col);
			if (relatedTerm.isEmpty())
				values.removeAt(row);
			else
			{
				QList<QString> newPair {QString(), QString()};
				newPair[1-col] = relatedTerm;
				values.replace(row, newPair);
			}
		}
	}
	
	_terms.set(values);

	endResetModel();

	emit modelChanged();
}

bool ListModelPairsAssigned::canAddTerms(Terms *terms) const
{
	for (const Term &variable : *terms)
	{
		if ( ! isAllowed(variable))
			return false;
	}

	return true;
}

Terms* ListModelPairsAssigned::addTerms(Terms *terms, int dropItemIndex)
{
	beginResetModel();
	Terms* removedTerms = new Terms();
	
	if (terms->size() == 0)
		return removedTerms;
	
	QList<QList<QString>> values = _terms.asQListOfQLists();
	bool done = false;
	if (terms->size() == 1 && dropItemIndex >= 0)
	{
		int realRow = dropItemIndex / 2;
		int realCol = dropItemIndex % 2;
		if (realRow < values.size())
		{
			QList<QString> onePair = values[realRow];
			QString result = onePair[realCol];
			if (!result.isEmpty())
				removedTerms->add(Term(result));
			QString term = QString::fromStdString(terms[0].asString());
			onePair[realCol] = term;
			values[realRow] = onePair;
			done = true;
		}
	}
	
	if (!done)
	{
		QList<QString> newValues = terms->asQList();
		int index = 0;
		for (int row = 0; row < values.length(); row++)
		{
			QList<QString> rowValues = values.at(row);
			if (rowValues[0].isEmpty())
			{
				rowValues[0] = newValues[index];
				values.replace(row, rowValues);
				index++;
			}
			if (index >= newValues.length())
				break;
			
			rowValues = values.at(row); //re-query the row in case it was changed
			if (rowValues[1].isEmpty())
			{
				rowValues[1] = newValues[index];
				values.replace(row, rowValues);
				index++;
			}
			if (index >= newValues.length())
				break;
		}
		
		while (index < newValues.length())
		{
			QList<QString> newPair {newValues[index], QString()};
			index++;
			if (index < newValues.length())
			{
				newPair[1] = newValues[index];
				index++;
			}
			values.push_back(newPair);
		}
	}
	
	_terms.set(values);
	endResetModel();

	emit modelChanged();

	return removedTerms;
}

void ListModelPairsAssigned::moveTerms(const QList<int> &indexes, int dropItemIndex)
{
	if (indexes.length() != 1)
		return;
	
	bool isChanged = false;
	int fromIndex = indexes[0];
	if (fromIndex == dropItemIndex)
		return;
	
	int fromRow = fromIndex / 2;
	int fromCol = fromIndex % 2;
	
	beginResetModel();
	QList<QList<QString>> values = _terms.asQListOfQLists();
	if (fromRow < values.size())
	{
		QList<QString>& fromPairValue = values[fromRow];
		QString fromValue = fromPairValue[fromCol];
		if (!fromValue.isEmpty())
		{
			if (dropItemIndex >= 0)
			{
				int dropRow = dropItemIndex / 2;
				int dropCol = dropItemIndex % 2;
				if (dropRow < values.size())
				{
					QList<QString>& dropPairValue = values[dropRow];
					QString dropValue = dropPairValue[dropCol];
					fromPairValue[fromCol] = dropValue;
					dropPairValue[dropCol] = fromValue;
					isChanged = true;
				}
			}
			else
			{
				QList<QString>& dropPairValue = values[values.size() - 1];
				if (dropPairValue[0].isEmpty())
					dropPairValue[0] = fromValue;
				else if (dropPairValue[1].isEmpty())
					dropPairValue[1] = fromValue;
				else
				{
					QList<QString> newRow {fromValue, QString()};
					values.push_back(newRow);
				}
				fromPairValue[fromCol] = QString();
				isChanged = true;
			}
			
			if (fromPairValue[fromCol].isEmpty())
			{
				if (fromPairValue[1-fromCol].isEmpty())
					values.removeAt(fromRow);
			}
		}
	}

	_terms.set(values);	
	endResetModel();
	
	if (isChanged)
		emit modelChanged();
}
