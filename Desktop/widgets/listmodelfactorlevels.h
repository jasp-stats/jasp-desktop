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

		FactorLevelItem(const QString& _value, bool _isVirtual, bool _isLevel) :
			value(_value), isVirtual(_isVirtual), isLevel(_isLevel) {}

		FactorLevelItem(const FactorLevelItem& item) : value(item.value), isVirtual(item.isVirtual), isLevel(item.isLevel) {}

        bool operator==(const FactorLevelItem& item) const
        {
			return item.isLevel == isLevel
				&& item.isVirtual == isVirtual
				&& item.value == value;
        }
		bool operator!=(const FactorLevelItem& item) const
		{
			return !(item == *this);
		}

		static FactorLevelItem dummyFactor;
	};

	QList<FactorLevelItem>	_items;
	Terms					_allLevelsCombinations;

	QStringList			_getAllFactors()													const;
	QString				_giveUniqueValue(const FactorLevelItem& item, const QString value)	const;
	bool				_isDeletable(const FactorLevelItem& item)							const;
	FactorLevelItem&	_getFactor(const FactorLevelItem& item)								const;
	void				_setAllLevelsCombinations();
	bool				_removeItem(int row);
};

#endif // LISTMODELFACTORLEVELS_H
