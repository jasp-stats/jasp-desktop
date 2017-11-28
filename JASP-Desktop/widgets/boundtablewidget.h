//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef BOUNDTABLEWIDGET_H
#define BOUNDTABLEWIDGET_H

#include <QTableWidget>

#include "bound.h"

#include "common.h"
#include "options/option.h"
#include "options/optionstable.h"
#include "options/optionvariables.h"
#include "options/optiondoublearray.h"
#include "options/optionstring.h"

#include "qutils.h"

class BoundTableWidget : public QTableWidget, public Bound
{
public:
	BoundTableWidget(QWidget *parent = 0);

	virtual void bindTo(Option *option) OVERRIDE;

	void updateTableValues();

private:
	OptionsTable *_boundTo;
	std::vector<Options *> _groups;
};

#endif // BOUNDTABLEWIDGET_H
