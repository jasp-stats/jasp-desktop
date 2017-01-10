//
// Copyright (C) 2016 University of Amsterdam
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

#ifndef RIBBONBASREGRESSION_H
#define RIBBONBASREGRESSION_H

#include "ribbonwidget.h"

namespace Ui {
class RibbonBASRegression;
}

class RibbonBASRegression : public RibbonWidget
{
	Q_OBJECT

public:
	explicit RibbonBASRegression(QWidget *parent = 0);
	~RibbonBASRegression();

private:
	Ui::RibbonBASRegression *ui;
};

#endif // RIBBONBASREGRESSION_H
