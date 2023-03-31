//
// Copyright (C) 2013-2021 University of Amsterdam
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

#ifndef LISTMODELGRIDINPUT_H
#define LISTMODELGRIDINPUT_H

#include "listmodeltableviewbase.h"

class ListModelGridInput : public ListModelTableViewBase
{
	Q_OBJECT

public:
	explicit ListModelGridInput(TableViewBase * parent);

	int		rowCount(	const QModelIndex & = QModelIndex())		const	override { return _rowCount;						}
	int		columnCount(const QModelIndex & = QModelIndex())		const	override { return _tableTerms.values.length();		}

	void	setup()															override;
	void	initTableTerms(const TableTerms& terms)							override;

	void	sourceTermsReset()												override;

private:
	void	_readSource();

	int		_rowCount = 0;

};

#endif // LISTMODELGRIDINPUT_H
