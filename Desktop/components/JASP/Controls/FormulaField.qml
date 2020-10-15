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

import QtQuick			2.11
import JASP.Controls	1.0
import JASP				1.0

TextField
{
					id:					formulaField
					value:				"0"
	property double realValue:			0
	property var	realValues:			[]
	property double	min:				-Infinity
	property double	max:				Infinity
	property int	inclusive:			JASP.MinMax
	property alias	defaultValue:		formulaField.value
	property bool	multiple:			false
	property bool	parseDefaultValue:	true
					inputType:			multiple ? "formulaArray" : "formula"
					fieldWidth:			jaspTheme.textFieldWidth / 2
}
