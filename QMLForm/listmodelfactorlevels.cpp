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
#include "qutils.h"
#include "log.h"
#include "factorlevellistbase.h"

using namespace std;

ListModelFactorLevels::FactorLevelItem ListModelFactorLevels::FactorLevelItem::dummyFactor("", true, false);

ListModelFactorLevels::ListModelFactorLevels(JASPListControl* listView)
	: ListModel(listView)
{
	_factorLevelList = qobject_cast<FactorLevelListBase*>(listView);
	_needsSource = false;
	_itemType = "fixedFactors";
}

int ListModelFactorLevels::rowCount(const QModelIndex &) const
{
	return _items.length();
}

QVariant ListModelFactorLevels::data(const QModelIndex &index, int role) const
{
	int row = index.row();
	
	if (row < 0 || row >= _items.length())
	{
		Log::log()  << "Unknown row " << row << " in ListModelFactorLevels. Length is " << _items.length() << std::endl;
		return QVariant();
	}
	
	const FactorLevelItem& item = _items.at(row);
	
	if (role == Qt::DisplayRole || role == ListModelFactorLevels::NameRole)
		return item.value;
	else if (role == ListModelFactorLevels::TypeRole)
		return item.isLevel ? "level" : "factor";
	else if (role == ListModelFactorLevels::VirtualRole)
		return item.isVirtual;
	else if (role == ListModelFactorLevels::DeletableRole)
		return _isDeletable(item);
	
	return ListModel::data(index, role);
}

void ListModelFactorLevels::initFactors(const vector<pair<string, vector<string> > > &factors)
{
	beginResetModel();
	
	_items.clear();
	for (const pair<string, vector<string> > &factor : factors)
	{
		_items.append(FactorLevelItem(tq(factor.first), false, false));
		
		int levelIndex = 1;
		for (const string& level : factor.second)
		{
			_items.append(FactorLevelItem(tq(level), false, true));
			levelIndex++;
		}
		
		// Append a virtual level
		_items.append(FactorLevelItem(_factorLevelList->levelPlaceHolder(), true, true));
	}
	
	// Append a virtual factor
	_items.append(FactorLevelItem(_factorLevelList->factorPlaceHolder(), true, false));

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

void ListModelFactorLevels::_setAllLevelsCombinations()
{
	vector<vector<string> > allLevelsCombinations;
	_setTerms(_getAllFactors()); // _terms get only the factors
	
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

QStringList ListModelFactorLevels::_getAllFactors() const
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
	if (row < 0 || row >= _items.length()) return;

	FactorLevelItem&	item	= _items[row];
	QString				val		= value.toString(),
						oldVal	= item.value;
	bool				updateRow = true;
	
	if (val.isEmpty())
		// Try to remove the row. If it is not possible, refresh the row
		updateRow = !_removeItem(row);
	else
	{
		if (!item.isVirtual && item.value == val) return;
	
		// Check first that the value is unique
		item.value = _giveUniqueValue(item, val);

		if (item.isVirtual)
		{
			// If the item was virtual, then it is not anymore
			item.isVirtual = false;

			if (item.isLevel)
			{
				// It the changed item was a virtual level, add after this one a virtual level.
				beginInsertRows(QModelIndex(), row+1, row+1);
				_items.insert(row + 1, FactorLevelItem(_factorLevelList->levelPlaceHolder(), true, true));
				endInsertRows();
			}
			else
			{
				// If the changed item was a virtual factor, add the minimm number of levels, add a virtual level, and add also a virtual factor.
				beginInsertRows(QModelIndex(), _items.length(), _items.length() + _factorLevelList->minLevels() + 1);

				for (int i = 0; i < _factorLevelList->minLevels(); i++)
					_items.push_back(FactorLevelItem(_factorLevelList->getLevelName(i + 1), false, true));

				_items.push_back(FactorLevelItem(_factorLevelList->levelPlaceHolder(), true, true)); // Virtual level
				_items.push_back(FactorLevelItem(_factorLevelList->factorPlaceHolder(), true, false)); // Virtual factor

				endInsertRows();
			}
		}
		else
			emit namesChanged({ {oldVal, item.value} });

		_setAllLevelsCombinations();
	}

	if (updateRow)
	{
		QModelIndex modelIndex = index(row, 0);
		emit dataChanged(modelIndex, modelIndex);
	}
}

bool ListModelFactorLevels::_removeItem(int row)
{
	if (row < 0 || row >= _items.length()) return false;

	const FactorLevelItem& item = _items[row];
	if (!_isDeletable(item)) return false;

	int nbRowsToRemove = 1;
	if (!item.isLevel)
	{
		// For a factor, remove the factor and all its levels
		while (row + nbRowsToRemove < _items.length())
		{
			if (_items.at(row + nbRowsToRemove).isLevel) nbRowsToRemove++;
			else break;
		}
	}

	beginRemoveRows(QModelIndex(), row, row + nbRowsToRemove - 1);
	for (int i = 0; i < nbRowsToRemove; i++)
		_items.removeAt(row);
	_setAllLevelsCombinations();
	endRemoveRows();

	return true;
}

void ListModelFactorLevels::itemRemoved(int row)
{
	_removeItem(row);
}

QString ListModelFactorLevels::_giveUniqueValue(const FactorLevelItem &item, const QString value) const
{
	QString result = value;
	QStringList otherValues;

	if (item.isLevel)
	{
		// Get all other levels that are in the same factor as item
		const FactorLevelItem& factor = _getFactor(item);
		int indexFactor = _items.indexOf(factor);
		for (int index = indexFactor + 1; index < _items.length(); index++)
		{
			const FactorLevelItem& level = _items[index];
			if (!level.isLevel || level.isVirtual) break;
			if (level != item) otherValues.push_back(level.value);
		}
	}
	else
	{
		// Get all other factors
		for (const FactorLevelItem& factor: _items)
		{
			if (!factor.isVirtual && !factor.isLevel && factor != item)
				otherValues.push_back(factor.value);
		}

	}

	// If the value is not unique, append '(1)' to it. But if '<value> (1)' exists also, append '(2)' instead. And so on...
	int i = 1;
	bool isUnique = false;
	while (!isUnique)
	{
		isUnique = true;
		for (const QString& anotherValue : otherValues)
			if (result == anotherValue) isUnique = false;
		if (!isUnique) result = value + tq(" (") + QString::number(i) + tq(")");
		i++;
	}
	return result;
}

bool ListModelFactorLevels::_isDeletable(const ListModelFactorLevels::FactorLevelItem &item) const
{
	if (item.isVirtual)
		return false;
	else if (item.isLevel)
		return (_items.indexOf(item) - _items.indexOf(_getFactor(item))) > _factorLevelList->minLevels();
	else
	{
		int count = 0;
		for (const FactorLevelItem& _factor : _items)
		{
			if (!_factor.isLevel) count++;
			if (_factor == item) break;
		}
		return count > _factorLevelList->minFactors();
	}
}

ListModelFactorLevels::FactorLevelItem &ListModelFactorLevels::_getFactor(const FactorLevelItem &item) const
{
	FactorLevelItem& factor = FactorLevelItem::dummyFactor;
	for (const FactorLevelItem& oneItem : _items)
	{
		if (!oneItem.isLevel) factor = oneItem;
		if (oneItem == item) break;
	}

	return factor;
}
