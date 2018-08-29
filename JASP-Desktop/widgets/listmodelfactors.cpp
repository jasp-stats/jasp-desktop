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

#include "listmodelfactors.h"
#include "analysis/options/options.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "utilities/qutils.h"

using namespace std;

ListModelFactors::ListModelFactors(AnalysisQMLForm *form, QQuickItem *item)
	: ListModel(form, item)
{
	_itemType = "fixedFactors";
	setTermsAreNotVariables();
	QObject::connect(_item, SIGNAL(itemChanged(int, QVariant)), this, SLOT(itemChanged(int, QVariant)));	
	QObject::connect(_item, SIGNAL(itemRemoved(int)), this, SLOT(itemRemoved(int)));	
}

int ListModelFactors::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	return _factors.length();
}

QVariant ListModelFactors::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	
	if (row >= _factors.length())
	{
		qDebug() << "Unknown row " << row << " in ListModelFactors. Length is " << _factors.length();
		return QVariant();
	}
	
	const Factor& factor = _factors.at(row);
	
	QString value;
	if (role == Qt::DisplayRole || role == ListModelFactors::NameRole)
	{
		value = factor.value;
	}
	else if (role == ListModelFactors::TypeRole)
	{
		QStringList listValues;
		if (factor.isVirtual)
			listValues.push_back(tq("Virtual"));
		if (factor.isLevel)
		{
			listValues.push_back(tq("Level"));
			if (factor.index > 2 && !factor.isVirtual)
				listValues.push_back(tq("Deletable"));
		}
		else
		{
			listValues.push_back(tq("Factor"));
			if (factor.index > 1 && !factor.isVirtual)
				listValues.push_back(tq("Deletable"));
		}
		value = listValues.join(',');
	}
	
	return QVariant(value);	
}

void ListModelFactors::initFactors(const vector<pair<string, vector<string> > > &factors)
{
	beginResetModel();
	
	_factors.clear();
	_factorTitles.clear();
	int factorIndex = 1;
	for (const pair<string, vector<string> > &factor : factors)
	{
		Factor factorHeader(tq(factor.first), false, false, factorIndex++);
		_factors.append(factorHeader);
		_factorTitles.append(factorHeader.value);
		
		int levelIndex = 1;
		for (const string& level : factor.second)
		{
			Factor factorLevel(tq(level), false, true, levelIndex++);
			_factors.append(factorLevel);
		}
		
		Factor extraLevel(tq("Level ") + levelIndex, true, true, levelIndex);
		_factors.append(extraLevel);
	}
	
	Factor extraFactor(tq("RM Factor ") + factorIndex, true, false, factorIndex);
	_factors.append(extraFactor);

	endResetModel();
	
	_setAllLevelsCombinations();	
}

vector<pair<string, vector<string> > > ListModelFactors::getFactors() const
{
	vector<pair<string, vector<string> > > result;
	string currentFactorName;
	vector<string> currentLevels;
	for (const Factor& factor: _factors)
	{
		if (!factor.isVirtual && !factor.isLevel)
		{
			currentFactorName = factor.value.toStdString();
			currentLevels.clear();
		} 
		else if (!factor.isVirtual && factor.isLevel)
		{
			currentLevels.push_back(factor.value.toStdString());
		}	
		else if (factor.isVirtual && factor.isLevel)
		{
			result.push_back(make_pair(currentFactorName, currentLevels));
			currentLevels.clear();
		}
	}
	
	return result;
}

const Terms &ListModelFactors::getLevels() const
{
	return _allLevelsCombinations;
}

const Terms& ListModelFactors::terms() const
{
	return _terms;
}

void ListModelFactors::_setAllLevelsCombinations()
{
	vector<vector<string> > allLevelsCombinations;
	_terms.set(_factorTitles);
	
	vector<vector<string> > allLevels;	
	vector<string> currentLevels;
	for (const Factor& factor: _factors)
	{
		if (factor.isLevel && !factor.isVirtual)
			currentLevels.push_back(factor.value.toStdString());
		else if (!currentLevels.empty())
		{
			allLevels.push_back(currentLevels);
			currentLevels.clear();
		}
	}
	
	for (const string& level : allLevels[0])
	{
		vector<string> levelVector {level};
		allLevelsCombinations.push_back(levelVector);
	}
	
	for (uint i = 1; i < allLevels.size(); i++)
	{
		const vector<string>& levels = allLevels[i];
		vector<vector<string> > previousLevelCombinations = allLevelsCombinations; // Copy it
		allLevelsCombinations.clear();
		
		for (uint j = 0; j < previousLevelCombinations.size(); j++)
		{
			for (uint k = 0; k < levels.size(); k++)
			{
				vector<string> previousLevels = previousLevelCombinations[j];
				previousLevels.push_back(levels[k]);
				allLevelsCombinations.push_back(previousLevels);
			}
		}		
	}
	
	_allLevelsCombinations.set(allLevelsCombinations);
}

void ListModelFactors::itemChanged(int row, QVariant value)
{
	if (row >= _factors.length())
	{
		qDebug() << "Index " << row << " in ListModelFactors is greater than the maximum " << _factors.length();
		return;
	}
	
	beginResetModel();
	
	Factor& factor = _factors[row];
	factor.value = value.toString();
	
	if (factor.value.isEmpty())
	{
		if (factor.isLevel)
		{
			if (factor.index > 2 && !factor.isVirtual)
				_factors.removeAt(row);
			else
				factor.value = tq("Level ") + factor.index;
		}
		else
		{
			if (factor.index > 1 && !factor.isVirtual)
			{
				_factorTitles.removeAt(factor.index - 1);
				_factors.removeAt(row);
				while (row < _factors.length())
				{
					factor = _factors.at(row);
					if (factor.isLevel)
						_factors.removeAt(row);
					else
						break;
				}
			}
			else
				factor.value = tq("RM Factor ") + factor.index;
		}
	}
	else if (factor.isVirtual)
	{
		factor.isVirtual = false;
		if (factor.isLevel)
		{
			Factor newLevel(tq("Level ") + factor.index + 1, true, true, factor.index + 1);
			_factors.insert(row + 1, newLevel);
		}
		else
		{
			_factorTitles.push_back(factor.value);
			Factor newLevel1(tq("Level 1"), false, true, 1);
			_factors.push_back(newLevel1);
			Factor newLevel2(tq("Level 2"), false, true, 2);
			_factors.push_back(newLevel2);
			Factor newVirtualLevel(tq("Level"), true, true, 3);
			_factors.push_back(newVirtualLevel);
			Factor newVirtualFactor(tq("Factor ") + factor.index + 1, true, false, factor.index + 1);
			_factors.push_back(newVirtualFactor);
		}
	}
	else if (!factor.isLevel)
		_factorTitles[factor.index - 1] = factor.value;
	
	endResetModel();
	
	_setAllLevelsCombinations();
	
	emit termsChanged();
}

void ListModelFactors::itemRemoved(int row)
{
	if (row >= _factors.length())
	{
		qDebug() << "Row " << row << " in ListModelFactors is greater than the maximum " << _factors.length();
		return;
	}
	
	beginResetModel();
	
	Factor& factor = _factors[row];
	
	if (!factor.isVirtual)
	{
		if (factor.isLevel)
		{
			if (factor.index > 2)
				_factors.removeAt(row);
		}
		else
		{
			if (factor.index > 1)
			{
				_factorTitles.removeAt(factor.index - 1);
				_factors.removeAt(row);
				while (row < _factors.length())
				{
					factor = _factors.at(row);
					if (factor.isLevel)
						_factors.removeAt(row);
					else
						break;
				}
			}
		}
	}
	
	endResetModel();
	
	_setAllLevelsCombinations();
	
	emit termsChanged();
}
