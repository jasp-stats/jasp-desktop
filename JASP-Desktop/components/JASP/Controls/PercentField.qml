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

TextField {
    id: textField
    property int defaultValue: 50
    property bool showPercent: true
    property bool with1Decimal: false
    
    property var intVal: IntValidator {bottom: 0; top: 100}
    property var doubleVal: DoubleValidator {bottom: 0; top: 100; decimals: 1}
    
    inputType: "percent"
    control.width: Theme.font.pixelSize * (with1Decimal ? 3 : 2)
    validator: with1Decimal ? doubleVal : intVal
    value: Number.parseInt(defaultValue);
    afterLabel.text: showPercent ? "%" : ""
}
