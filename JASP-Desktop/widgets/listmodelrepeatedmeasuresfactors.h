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
		int			index;
		Factor(const QString& _value, bool _isVirtual, bool _isLevel, int _index) :
			value(_value), isVirtual(_isVirtual), isLevel(_isLevel), index(_index) {}
	};
	QList<Factor>	_factors;
	QList<QString>	_factorTitles;
	Terms			_allLevelsCombinations;
	
	void _setAllLevelsCombinations();
};

#endif // LISTMODELREPEATEDMEASURESFACTORS_H
