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
					id:					textField
	property int	defaultValue:		0
	property int	_prevDefaultValue:	0
	property bool	negativeValues:		false
	property int	min:				negativeValues ? -2147483647 : 0 // 2^32 - 1
	property int	max:				2147483647
	property alias	inclusive:			intValidator.inclusive
	property alias	intValidator:		intValidator
    
					inputType:			"integer"
					validator:			JASPDoubleValidator { id: intValidator; bottom: min; top: max; decimals: 0 }
					lastValidValue:		defaultValue;
					value:				defaultValue
					cursorShape:		Qt.IBeamCursor
					fieldWidth:			jaspTheme.numericFieldWidth

	onDefaultValueChanged:
	{
		if (_prevDefaultValue == value)
			value = defaultValue;

		_prevDefaultValue = defaultValue;
	}
}
