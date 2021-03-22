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

#ifndef LISTMODELFACTORLEVELS_H
#define LISTMODELFACTORLEVELS_H

#include "listmodel.h"

class FactorLevelListBase;

class ListModelFactorLevels : public ListModel
{
	Q_OBJECT
public:
	
	ListModelFactorLevels(JASPListControl* listView);
	
	int rowCount(const QModelIndex &parent = QModelIndex())						const override;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole)			const override;
	
	void initFactors(const std::vector<std::pair<std::string, std::vector<std::string> > > &factors);
	std::vector<std::pair<std::string, std::vector<std::string> > > getFactors() const;
	const Terms& getLevels() const;

	
public slots:
	void itemChanged(int row, QVariant value);
	void itemRemoved(int row);
		
protected:
	FactorLevelListBase* _factorLevelList = nullptr;

	struct FactorLevelItem
	{
		QString				value;
		bool				isVirtual;
		bool				isLevel;
		FactorLevelItem*	headFactor;
		FactorLevelItem(const QString& _value, bool _isVirtual, bool _isLevel, FactorLevelItem* _factor = nullptr) :
			value(_value), isVirtual(_isVirtual), isLevel(_isLevel)
		{
			if (_factor)
				headFactor = _factor;
			else
				headFactor = this;
		}

		FactorLevelItem(const FactorLevelItem& item) : value(item.value), isVirtual(item.isVirtual), isLevel(item.isLevel)
		{
			if (&item == item.headFactor)
				headFactor = this;
			else
				headFactor = item.headFactor;
		}

		bool operator==(const FactorLevelItem& item)
		{
			return item.headFactor == headFactor
					&& item.isLevel == isLevel
					&& item.isVirtual == isVirtual
					&& item.value == value;
		}
	};
	QList<FactorLevelItem>	_items;
	Terms					_allLevelsCombinations;

	QStringList		_getOtherLevelsStringList(const FactorLevelItem& item);
	QStringList		_getAllFactorsStringList();
	QString			_giveUniqueName(const QStringList& names, const QString startName);
	int				_getIndex(const FactorLevelItem& item) const;
	
	void			_updateVirtualLevelIndex(FactorLevelItem* headFactor);
	void			_updateVirtualFactorIndex();
	void			_setAllLevelsCombinations();
	QString			_removeItem(int row);
};

#endif // LISTMODELFACTORLEVELS_H
