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

import QtQuick 2.11
import JASP.Controls 1.0
import JASP.Theme 1.0
import JASP 1.0

TextField
{
	property int	defaultValue:	50
	property bool	showPercent:	true
	property int	decimals:		0
    
    
	id:					percentField
	inputType:			"percent"
	fieldWidth:			Theme.font.pixelSize * (percentField.decimals + 3)
	validator:			JASPDoubleValidator { id: doubleValidator; bottom: 0; top: 100; decimals: percentField.decimals; notation: DoubleValidator.StandardNotation}

	value:				Number.parseInt(defaultValue);
	afterLabel:			showPercent ? "%" : ""
	cursorShape:		Qt.IBeamCursor	
}
