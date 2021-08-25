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

#include "listmodelfactorlevels.h"
#include "utilities/qutils.h"
#include "log.h"
#include "factorlevellistbase.h"

using namespace std;

ListModelFactorLevels::ListModelFactorLevels(JASPListControl* listView)
	: ListModel(listView)
{
	_factorLevelList = qobject_cast<FactorLevelListBase*>(listView);
	_needsSource = false;
	_itemType = "fixedFactors";
}

int ListModelFactorLevels::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);
	return _items.length();
}

QVariant ListModelFactorLevels::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	
	if (row >= _items.length())
	{
		Log::log()  << "Unknown row " << row << " in ListModelFactorLevels. Length is " << _items.length() << std::endl;
		return QVariant();
	}
	
	const FactorLevelItem& factor = _items.at(row);
	int factorIndex = _getIndex(factor);
	
	if (role == Qt::DisplayRole || role == ListModelFactorLevels::NameRole)
		return factor.value;
	else if (role == ListModelFactorLevels::TypeRole)
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

void ListModelFactorLevels::initFactors(const vector<pair<string, vector<string> > > &factors)
{
	beginResetModel();
	
	_items.clear();
	for (const pair<string, vector<string> > &factor : factors)
	{
		FactorLevelItem factorHeader(tq(factor.first), false, false);
		_items.append(factorHeader);
		FactorLevelItem& factorHeaderRef = _items[_items.length() - 1];
		
		int levelIndex = 1;
		for (const string& level : factor.second)
		{
			FactorLevelItem factorLevel(tq(level), false, true, &factorHeaderRef);
			_items.append(factorLevel);
			levelIndex++;
		}
		
		FactorLevelItem extraLevel(_factorLevelList->levelPlaceHolder(), true, true, &factorHeaderRef);
		_items.append(extraLevel);
	}
	
	FactorLevelItem extraFactor(_factorLevelList->factorPlaceHolder(), true, false);
	_items.append(extraFactor);

	_setAllLevelsCombinations();

	endResetModel();
	
}

vector<pair<string, vector<string> > > ListModelFactorLevels::getFactors() const
{
	vector<pair<string, vector<string> > > result;
	string currentFactorName;
	vector<string> currentLevels;
	for (const FactorLevelItem& factor: _items)
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

const Terms &ListModelFactorLevels::getLevels() const
{
	return _allLevelsCombinations;
}

void ListModelFactorLevels::_updateVirtualLevelIndex(FactorLevelItem* headFactor)
{
	int row = _items.indexOf(*headFactor);
	int levelIndex = 1;
	for (int i = row + 1; i < _items.length(); i++, levelIndex++)
	{
		FactorLevelItem& factor = _items[i];
		if (factor.headFactor != headFactor)
			break;
		if (factor.isVirtual)
		{
			factor.value = _factorLevelList->getLevelName(levelIndex);
			QModelIndex modelIndex = index(i, 0);
			emit dataChanged(modelIndex, modelIndex);
		}
	}
}

void ListModelFactorLevels::_updateVirtualFactorIndex()
{
	int factorIndex = 1;
	for (FactorLevelItem& factor: _items)
	{
		if (!factor.isLevel)
		{
			if (factor.isVirtual)
			{
				int row = _items.indexOf(factor);
				factor.value = _factorLevelList->getFactorName(factorIndex);
				QModelIndex modelIndex = index(row, 0);
				emit dataChanged(modelIndex, modelIndex);
			}
			else
				factorIndex++;
		}
	}
}

void ListModelFactorLevels::_setAllLevelsCombinations()
{
	vector<vector<string> > allLevelsCombinations;
	_setTerms(_getAllFactorsStringList());
	
	vector<vector<string> > allLevels;	
	vector<string> currentLevels;
	for (const FactorLevelItem& factor: _items)
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

QStringList ListModelFactorLevels::_getAllFactorsStringList()
{
	QStringList result;

	for (const FactorLevelItem& factor: _items)
	{
		if (!factor.isVirtual && !factor.isLevel)
			result.push_back(factor.value);
	}

	return result;
}

void ListModelFactorLevels::itemChanged(int row, QVariant value)
{
	if (row >= _items.length())
	{
		Log::log()  << "Index " << row << " in ListModelFactors is greater than the maximum " << _items.length() << std::endl;
		return;
	}

	FactorLevelItem&	item	= _items[row];
	QString				val		= value.toString(),
						oldVal	= item.value;
	int				itemIndex	= _getIndex(item);
	
	if ((!item.isVirtual && item.value == val) || (item.isVirtual && val.isEmpty()))
		return;
	
	if (val.isEmpty() && !item.isVirtual)
		val = _removeItem(row);

	if (!val.isEmpty())
	{
		if (item.isLevel)
		{
			QStringList levels = _getOtherLevelsStringList(item);
			val = _giveUniqueName(levels, val);
		}
		else
		{
			QStringList allFactors = _getAllFactorsStringList();
			allFactors.removeAt(itemIndex - 1);
			val = _giveUniqueName(allFactors, val);
		}

		item.value = val;

		if (item.isVirtual)
		{
			item.isVirtual = false;
			if (item.isLevel)
			{
				beginInsertRows(QModelIndex(), row+1, row+1);
				FactorLevelItem newLevel(_factorLevelList->getLevelName(itemIndex + 1), true, true, item.headFactor);
				_items.insert(row + 1, newLevel);
				endInsertRows();
			}
			else
			{
				beginInsertRows(QModelIndex(), _items.length(), _items.length() + 3);

				int i = 0;
				for (; i < _factorLevelList->minLevels(); i++)
				{
					FactorLevelItem newLevel(_factorLevelList->getLevelName(i + 1), false, true, &item);
					_items.push_back(newLevel);
				}
				FactorLevelItem newVirtualLevel(_factorLevelList->getLevelName(i + 1), true, true, &item);
				_items.push_back(newVirtualLevel);
				FactorLevelItem newVirtualFactor(_factorLevelList->getFactorName(itemIndex + 1), true, false);
				_items.push_back(newVirtualFactor);

				endInsertRows();
			}
		}
		else
			emit namesChanged({ {oldVal, val} });

		_setAllLevelsCombinations();
		QModelIndex modelIndex = index(row, 0);
		emit dataChanged(modelIndex, modelIndex);
	}
	
}

QString ListModelFactorLevels::_removeItem(int row)
{
	QString value;

	if (row >= _items.length())
		return value;

	const FactorLevelItem& item = _items[row];
	if (item.isVirtual) return value;

	int itemIndex = _getIndex(item);
	if (item.isLevel)
	{
		if (itemIndex > _factorLevelList->minLevels())
		{
			FactorLevelItem* factor = item.headFactor;
			beginRemoveRows(QModelIndex(), row, row);
			_items.removeAt(row);
			_setAllLevelsCombinations();
			endRemoveRows();
			_updateVirtualLevelIndex(factor);
		}
		else
			value = _factorLevelList->getLevelName(itemIndex);
	}
	else
	{
		if (itemIndex > _factorLevelList->minFactors())
		{
			int countRemoved = 1;
			while (row + countRemoved < _items.length())
			{
				const FactorLevelItem& factor2 = _items.at(row + countRemoved);
				if (factor2.isLevel)
					countRemoved++;
				else
					break;
			}

			beginRemoveRows(QModelIndex(), row, row + countRemoved - 1);
			for (int i = 0; i < countRemoved; i++)
				_items.removeAt(row);
			_setAllLevelsCombinations();
			endRemoveRows();

			_updateVirtualFactorIndex();
		}
		else
			value = _factorLevelList->getFactorName(itemIndex);
	}

	return value;
}

void ListModelFactorLevels::itemRemoved(int row)
{
	_removeItem(row);
}

QStringList ListModelFactorLevels::_getOtherLevelsStringList(const FactorLevelItem& item)
{
	QStringList result;
	for (const FactorLevelItem& factor : _items)
	{
		if (!factor.isVirtual && factor.isLevel && (&factor != &item) && factor.headFactor == item.headFactor)
			result.push_back(factor.value);
	}

	return result;
}

QString ListModelFactorLevels::_giveUniqueName(const QStringList &names, const QString startName)
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

int ListModelFactorLevels::_getIndex(const ListModelFactorLevels::FactorLevelItem &item) const
{
	int index = 1;
	if (item.isLevel)
		index =  _items.indexOf(item) - _items.indexOf(*(item.headFactor));
	else
	{
		for (const FactorLevelItem& _factor : _items)
		{
			if (_factor.headFactor == item.headFactor)
				return index;
			if (!_factor.isLevel)
				index++;
		}
	}

	return index;
}
