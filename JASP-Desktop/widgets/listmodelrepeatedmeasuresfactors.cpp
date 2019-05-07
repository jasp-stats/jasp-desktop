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

#include "listmodelrepeatedmeasuresfactors.h"
#include "analysis/options/options.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "utilities/qutils.h"
#include "log.h"



using namespace std;

ListModelRepeatedMeasuresFactors::ListModelRepeatedMeasuresFactors(QMLListView* listView)
	: ListModel(listView)
{
	_itemType = "fixedFactors";
}

int ListModelRepeatedMeasuresFactors::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	return _factors.length();
}

QVariant ListModelRepeatedMeasuresFactors::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	
	if (row >= _factors.length())
	{
		Log::log()  << "Unknown row " << row << " in ListModelFactors. Length is " << _factors.length() << std::endl;
		return QVariant();
	}
	
	const Factor& factor = _factors.at(row);
	
	QString value;
	if (role == Qt::DisplayRole || role == ListModelRepeatedMeasuresFactors::NameRole)
	{
		value = factor.value;
	}
	else if (role == ListModelRepeatedMeasuresFactors::TypeRole)
	{
		QStringList listValues;
		if (factor.isVirtual)
			listValues.push_back(tq("virtual"));
		if (factor.isLevel)
		{
			listValues.push_back(tq("level"));
			if (factor.index > 2 && !factor.isVirtual)
				listValues.push_back(tq("deletable"));
		}
		else
		{
			listValues.push_back(tq("factor"));
			if (factor.index > 1 && !factor.isVirtual)
				listValues.push_back(tq("deletable"));
		}
		value = listValues.join(',');
	}
	
	return QVariant(value);	
}

void ListModelRepeatedMeasuresFactors::initFactors(const vector<pair<string, vector<string> > > &factors)
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
			Factor factorLevel(tq(level), false, true, levelIndex++, &factorHeader);
			_factors.append(factorLevel);
		}
		
		Factor extraLevel(tq("Level ") + levelIndex, true, true, levelIndex, &factorHeader);
		_factors.append(extraLevel);
	}
	
	Factor extraFactor(tq("RM Factor ") + factorIndex, true, false, factorIndex);
	_factors.append(extraFactor);

	endResetModel();
	
	_setAllLevelsCombinations();	
}

vector<pair<string, vector<string> > > ListModelRepeatedMeasuresFactors::getFactors() const
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

const Terms &ListModelRepeatedMeasuresFactors::getLevels() const
{
	return _allLevelsCombinations;
}

void ListModelRepeatedMeasuresFactors::_setAllLevelsCombinations()
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

void ListModelRepeatedMeasuresFactors::itemChanged(int row, QVariant value)
{
	if (row >= _factors.length())
	{
		Log::log()  << "Index " << row << " in ListModelFactors is greater than the maximum " << _factors.length() << std::endl;
		return;
	}

	QString val = value.toString();
	Factor& factor = _factors[row];
	
	if (factor.value == val || (factor.isVirtual && val.isEmpty()))
		return;
	
	beginResetModel();

	bool setValue = true;
	bool isEmpty = false;
	
	if (factor.value.isEmpty())
	{
		isEmpty = true;
		if (factor.isLevel)
		{
			if (factor.index > 2 && !factor.isVirtual)
			{
				setValue = false;
				_factors.removeAt(row);
			}
			else
				val = tq("Level ") + QString::number(factor.index);
		}
		else
		{
			if (factor.index > 1 && !factor.isVirtual)
			{
				setValue = false;
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
				val = tq("RM Factor ") + QString::number(factor.index);
		}
	}

	if (setValue)
	{
		if (factor.isLevel)
		{
			QStringList levels = _getLevels(factor);
			val = _giveUniqueName(levels, val);
		}
		else
		{
			QStringList allFactors = _getAllFactors();
			val = _giveUniqueName(allFactors, val);
		}
		factor.value = val;
	}

	if (!isEmpty)
	{
		if (factor.isVirtual)
		{
			factor.isVirtual = false;
			if (factor.isLevel)
			{
				Factor newLevel(tq("Level ") + QString::number(factor.index + 1), true, true, factor.index + 1, factor.headFactor);
				_factors.insert(row + 1, newLevel);
			}
			else
			{
				_factorTitles.push_back(factor.value);
				Factor newLevel1(tq("Level 1"), false, true, 1, &factor);
				_factors.push_back(newLevel1);
				Factor newLevel2(tq("Level 2"), false, true, 2, &factor);
				_factors.push_back(newLevel2);
				Factor newVirtualLevel(tq("Level"), true, true, 3, &factor);
				_factors.push_back(newVirtualLevel);
				Factor newVirtualFactor(tq("Factor ") + QString::number(factor.index + 1), true, false, factor.index + 1);
				_factors.push_back(newVirtualFactor);
			}
		}
		else if (!factor.isLevel)
			_factorTitles[factor.index - 1] = factor.value;
	}
	
	endResetModel();
	
	_setAllLevelsCombinations();
	
	emit modelChanged();
}

void ListModelRepeatedMeasuresFactors::itemRemoved(int row)
{
	if (row >= _factors.length())
	{
		Log::log()  << "Row " << row << " in ListModelFactors is greater than the maximum " << _factors.length() << std::endl;
		return;
	}
	
	beginResetModel();
	
	const Factor& factor = _factors[row];
	
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
					const Factor& factor2 = _factors.at(row);
					if (factor2.isLevel)
						_factors.removeAt(row);
					else
						break;
				}
			}
		}
	}
	
	endResetModel();
	
	_setAllLevelsCombinations();
	
	emit modelChanged();
}

QStringList ListModelRepeatedMeasuresFactors::_getLevels(const Factor& item)
{
	QStringList result;
	for (const Factor& factor : _factors)
	{
		if (factor.headFactor == item.headFactor)
			result.push_back(factor.value);
	}

	return result;
}

QStringList ListModelRepeatedMeasuresFactors::_getAllFactors()
{
	QStringList result;
	for (const Factor& factor : _factors)
	{
		if (!factor.isLevel)
			result.push_back(factor.value);
	}

	return result;
}

QString ListModelRepeatedMeasuresFactors::_giveUniqueName(const QStringList &names, const QString startName)
{
	int i = 1;
	bool isUnique = false;
	QString result = startName;
	while (!isUnique)
	{
		isUnique=  true;
		for (const QString& name : names)
		{
			if (result == name)
				isUnique = false;
		}
		if (!isUnique)
			result = startName + tq(" (") + QString::number(i) + tq(")");
		i++;
	}
	return result;
}
