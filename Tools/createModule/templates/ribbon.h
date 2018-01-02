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

#ifndef RIBBON{0}_H
#define RIBBON{0}_H

#include "ribbonwidget.h"

namespace Ui {{
	class Ribbon{1};
}}

class Ribbon{1} : public RibbonWidget
{{
	Q_OBJECT

public:
	explicit Ribbon{1}(QWidget *parent = 0);
	~Ribbon{1}();

private:
	Ui::Ribbon{1} *ui;
}};

#endif // RIBBON{0}_H
