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

#include "ribbon{2}.h"
#include "ui_ribbon{2}.h"

///// additional Headers

Ribbon{1}::Ribbon{1}(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::Ribbon{1})
{{
	ui->setupUi(this);
///// Ribbon Buttons and Menu
}}

Ribbon{1}::~Ribbon{1}()
{{
	delete ui;
}}
