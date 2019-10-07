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

#ifndef LISTMODELNETWORKFACTORS_H
#define LISTMODELNETWORKFACTORS_H

#include "listmodel.h"

class ListModelNetworkFactors : public ListModel
{
	Q_OBJECT
public:
	
	ListModelNetworkFactors(QMLListView* listView);
	
	int							rowCount(const QModelIndex &parent = QModelIndex())				const	override;
	QVariant					data(const QModelIndex &index, int role = Qt::DisplayRole)		const	override;
	void						initGroups(const std::vector<std::string> &groups);
	std::vector<std::string>	getGroups();


public slots:
	void itemChanged(int row, QVariant value);
	void itemRemoved(int row);
		
protected:

	struct Group
	{
		QString		value;
		bool		isVirtual;

		Group(const QString&		_value,	bool _isVirtual) : value(_value),							isVirtual(_isVirtual) {}
		Group(const std::string&	_value,	bool _isVirtual) : value(QString::fromStdString(_value)),	isVirtual(_isVirtual) {}
	};
	QList<Group>	_groups;
	QString			_removeGroup(int row);

};

#endif // LISTMODELNETWORKFACTORS_H
