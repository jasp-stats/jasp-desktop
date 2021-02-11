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
#include "utilities/qutils.h"
#include "log.h"


using namespace std;

ListModelRepeatedMeasuresFactors::ListModelRepeatedMeasuresFactors(JASPListControl* listView)
	: ListModel(listView)
{
	_needsSource = false;
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
	int factorIndex = _getIndex(factor);
	
	if (role == Qt::DisplayRole || role == ListModelRepeatedMeasuresFactors::NameRole)
		return factor.value;
	else if (role == ListModelRepeatedMeasuresFactors::TypeRole)
	{
		QStringList listValues;
		if (factor.isVirtual)
			listValues.push_back(tq("virtual"));
		if (factor.isLevel)
		{
			listValues.push_back(tq("level"));
			if (factorIndex > 2 && !factor.isVirtual)
				listValues.push_back(tq("deletable"));
		}
		else
		{
			listValues.push_back(tq("factor"));
			if (factorIndex > 1 && !factor.isVirtual)
				listValues.push_back(tq("deletable"));
		}
		return listValues.join(',');
	}
	
	return ListModel::data(index, role);
}

void ListModelRepeatedMeasuresFactors::initFactors(const vector<pair<string, vector<string> > > &factors)
{
	beginResetModel();
	
	_factors.clear();
	int factorIndex = 1;
	for (const pair<string, vector<string> > &factor : factors)
	{
		Factor factorHeader(tq(factor.first), false, false);
		_factors.append(factorHeader);
		Factor& factorHeaderRef = _factors[_factors.length() - 1];
		
		int levelIndex = 1;
		for (const string& level : factor.second)
		{
			Factor factorLevel(tq(level), false, true, &factorHeaderRef);
			_factors.append(factorLevel);
			levelIndex++;
		}
		
		Factor extraLevel(tr("Level %1").arg(levelIndex), true, true, &factorHeaderRef);
		_factors.append(extraLevel);
		factorIndex++;
	}
	
	Factor extraFactor(tr("RM Factor %1").arg(factorIndex), true, false);
	_factors.append(extraFactor);

	_setAllLevelsCombinations();

	endResetModel();
	
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

void ListModelRepeatedMeasuresFactors::_updateVirtualLevelIndex(Factor* headFactor)
{
	int row = _factors.indexOf(*headFactor);
	int levelIndex = 1;
	for (int i = row + 1; i < _factors.length(); i++, levelIndex++)
	{
		Factor& factor = _factors[i];
		if (factor.headFactor != headFactor)
			break;
		if (factor.isVirtual)
		{
			factor.value = tr("Level %1").arg(levelIndex);
			QModelIndex modelIndex = index(i, 0);
			emit dataChanged(modelIndex, modelIndex);
		}
	}
}

void ListModelRepeatedMeasuresFactors::_updateVirtualFactorIndex()
{
	int factorIndex = 1;
	for (Factor& factor: _factors)
	{
		if (!factor.isLevel)
		{
			if (factor.isVirtual)
			{
				int row = _factors.indexOf(factor);
				factor.value = tr("RM Factor %1").arg(factorIndex);
				QModelIndex modelIndex = index(row, 0);
				emit dataChanged(modelIndex, modelIndex);
			}
			else
				factorIndex++;
		}
	}
}

void ListModelRepeatedMeasuresFactors::_setAllLevelsCombinations()
{
	vector<vector<string> > allLevelsCombinations;
	_setTerms(_getAllFactorsStringList());
	
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

QStringList ListModelRepeatedMeasuresFactors::_getAllFactorsStringList()
{
	QStringList result;

	for (const Factor& factor: _factors)
	{
		if (!factor.isVirtual && !factor.isLevel)
			result.push_back(factor.value);
	}

	return result;
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
	int		factorIndex = _getIndex(factor);
	
	if ((!factor.isVirtual && factor.value == val) || (factor.isVirtual && val.isEmpty()))
		return;
	
	if (val.isEmpty() && !factor.isVirtual)
		val = _removeFactor(row);

	if (!val.isEmpty())
	{
		if (factor.isLevel)
		{
			QStringList levels = _getOtherLevelsStringList(factor);
			val = _giveUniqueName(levels, val);
		}
		else
		{
			QStringList allFactors = _getAllFactorsStringList();
			allFactors.removeAt(factorIndex - 1);
			val = _giveUniqueName(allFactors, val);
		}

		factor.value = val;

		if (factor.isVirtual)
		{
			factor.isVirtual = false;
			if (factor.isLevel)
			{
				beginInsertRows(QModelIndex(), row+1, row+1);
				Factor newLevel(tr("Level %1").arg(factorIndex + 1), true, true, factor.headFactor);
				_factors.insert(row + 1, newLevel);
				endInsertRows();
			}
			else
			{
				beginInsertRows(QModelIndex(), _factors.length(), _factors.length() + 3);
				Factor newLevel1(tr("Level %1").arg(1), false, true, &factor);
				_factors.push_back(newLevel1);
				Factor newLevel2(tr("Level %1").arg(2), false, true, &factor);
				_factors.push_back(newLevel2);
				Factor newVirtualLevel(tr("Level %1").arg(3), true, true, &factor);
				_factors.push_back(newVirtualLevel);
				Factor newVirtualFactor(tr("RM Factor %1").arg(factorIndex + 1), true, false);
				_factors.push_back(newVirtualFactor);
				endInsertRows();
			}
		}

		_setAllLevelsCombinations();
		QModelIndex modelIndex = index(row, 0);
		emit dataChanged(modelIndex, modelIndex);
	}
	
}

QString ListModelRepeatedMeasuresFactors::_removeFactor(int row)
{
	QString value;

	if (row >= _factors.length())
		return value;

	const Factor& factor = _factors[row];
	int factorIndex = _getIndex(factor);
	if (factor.isLevel)
	{
		if (factorIndex > 2)
		{
			beginRemoveRows(QModelIndex(), row, row);
			_factors.removeAt(row);
			_setAllLevelsCombinations();
			endRemoveRows();
			_updateVirtualLevelIndex(factor.headFactor);
		}
		else
			value = tr("Level %1").arg(factorIndex);
	}
	else
	{
		if (factorIndex > 1)
		{
			int countRemoved = 1;
			while (row + countRemoved < _factors.length())
			{
				const Factor& factor2 = _factors.at(row + countRemoved);
				if (factor2.isLevel)
					countRemoved++;
				else
					break;
			}

			beginRemoveRows(QModelIndex(), row, row + countRemoved - 1);
			for (int i = 0; i < countRemoved; i++)
				_factors.removeAt(row);
			_setAllLevelsCombinations();
			endRemoveRows();

			_updateVirtualFactorIndex();
		}
		else
			value = tr("RM Factor %1").arg(factorIndex);
	}

	return value;
}

void ListModelRepeatedMeasuresFactors::itemRemoved(int row)
{
	_removeFactor(row);
}

QStringList ListModelRepeatedMeasuresFactors::_getOtherLevelsStringList(const Factor& item)
{
	QStringList result;
	for (const Factor& factor : _factors)
	{
		if (!factor.isVirtual && factor.isLevel && (&factor != &item) && factor.headFactor == item.headFactor)
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

int ListModelRepeatedMeasuresFactors::_getIndex(const ListModelRepeatedMeasuresFactors::Factor &factor) const
{
	int index = 1;
	if (factor.isLevel)
		index =  _factors.indexOf(factor) - _factors.indexOf(*(factor.headFactor));
	else
	{
		for (const Factor& _factor : _factors)
		{
			if (_factor.headFactor == factor.headFactor)
				return index;
			if (!_factor.isLevel)
				index++;
		}
	}

	return index;
}
