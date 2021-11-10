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
					id:					doubleField
					defaultValue:		0
	property string _prevDefaultValue:	"0"
	property alias	doubleValidator:	doubleValidator
	property bool	negativeValues:		false
	property double	min:				negativeValues ? -Infinity : 0
	property double	max:				Infinity
	property int	decimals:			3
	property alias	inclusive:			doubleValidator.inclusive

					inputType:			"number"
					validator:			JASPDoubleValidator { id: doubleValidator; bottom: min; top: max ; decimals: doubleField.decimals; notation: DoubleValidator.StandardNotation }
					fieldWidth:			jaspTheme.numericFieldWidth

	onDefaultValueChanged:
	{
		if (_prevDefaultValue === value)
		{
			value = defaultValue
			doEditingFinished()
		}

		_prevDefaultValue = defaultValue;
	}
}
