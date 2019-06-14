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

#ifndef LISTMODELREPEATEDMEASURESFACTORS_H
#define LISTMODELREPEATEDMEASURESFACTORS_H

#include "listmodel.h"

class ListModelRepeatedMeasuresFactors : public ListModel
{
	Q_OBJECT
public:
	
	ListModelRepeatedMeasuresFactors(QMLListView* listView);
	
	int rowCount(const QModelIndex &parent = QModelIndex())						const override;
	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole)			const override;
	
	void initFactors(const std::vector<std::pair<std::string, std::vector<std::string> > > &factors);
	std::vector<std::pair<std::string, std::vector<std::string> > > getFactors() const;
	const Terms& getLevels() const;

	
public slots:
	void itemChanged(int row, QVariant value);
	void itemRemoved(int row);
		
protected:
	struct Factor
	{
		QString		value;
		bool		isVirtual;
		bool		isLevel;
		Factor*		headFactor;
		Factor(const QString& _value, bool _isVirtual, bool _isLevel, Factor* _factor = nullptr) :
			value(_value), isVirtual(_isVirtual), isLevel(_isLevel)
		{
			if (_factor)
				headFactor = _factor;
			else
				headFactor = this;
		}

		Factor(const Factor& factor) : value(factor.value), isVirtual(factor.isVirtual), isLevel(factor.isLevel)
		{
			if (&factor == factor.headFactor)
				headFactor = this;
			else
				headFactor = factor.headFactor;
		}

		bool operator==(const Factor& factor)
		{
			return factor.headFactor == headFactor
					&& factor.isLevel == isLevel
					&& factor.isVirtual == isVirtual
					&& factor.value == value;
		}
	};
	QList<Factor>	_factors;
	Terms			_allLevelsCombinations;

	QStringList		_getOtherLevelsStringList(const Factor& factor);
	QStringList		_getAllFactorsStringList();
	QString			_giveUniqueName(const QStringList& names, const QString startName);
	int				_getIndex(const Factor& factor) const;
	
	void			_updateVirtualLevelIndex(Factor* headFactor);
	void			_updateVirtualFactorIndex();
	void			_setAllLevelsCombinations();
	QString			_removeFactor(int row);
};

#endif // LISTMODELREPEATEDMEASURESFACTORS_H
