//
// Copyright (C) 2013-2020 University of Amsterdam
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

#ifndef LISTMODELTABLEVIEWSIMPLE_H
#define LISTMODELTABLEVIEWSIMPLE_H

#include "listmodeltableviewbase.h"

class ListModelTableViewSimple : public ListModelTableViewBase
{
    Q_OBJECT
public:
    explicit ListModelTableViewSimple(TableViewBase * parent, QString tableType);

	OptionsTable*	createOption()							override;
	QString			getDefaultColName(size_t index)	const	override;

private:
    void _readValues();
};

#endif // LISTMODELTABLEVIEWSIMPLE_H
