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

#ifndef LISTMODELINPUTVALUE_H
#define LISTMODELINPUTVALUE_H

#include "listmodel.h"

class ListModelInputValue : public ListModel
{
	Q_OBJECT
public:
	
	ListModelInputValue(JASPListControl* listView, int minRows = 0);
	
	int							rowCount(const QModelIndex &parent = QModelIndex())				const	override;
	QVariant					data(const QModelIndex &index, int role = Qt::DisplayRole)		const	override;
	void						setAddVirtual(bool addVirtual, QString placeholder = "") { _addVirtual = addVirtual; _placeholder = placeholder; }

public slots:
	void itemChanged(int row, QVariant value);
	void itemRemoved(int row);
		
protected:
	void			_removeTerm(int row);
	QString			_makeUnique(const QString& val, int row = -1);
	QString			_changeLastNumber(const QString& val);

	bool			_addVirtual = true;
	int				_minRows = 0;
	QString			_placeholder = tr("New Value");

};

#endif // LISTMODELINPUTVALUE_H
