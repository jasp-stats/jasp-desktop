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
import JASPTheme 1.0

FocusScope {
    property int defaultValue: 50
    property bool showPercent: true
    property bool with1Decimal: false
    property alias name : textField.name
    property alias control: textField
    property alias label: textField.beforeLabel
    
    implicitWidth: textField.implicitWidth
    implicitHeight: textField.implicitHeight
    
    property var intVal: IntValidator {bottom: 0; top: 100}
    property var doubleVal: DoubleValidator {bottom: 0; top: 100; decimals: 1}
    
    TextField {
        id: textField
        inputType: "percent"
        control.width: Theme.font.pixelSize * (with1Decimal ? 3 : 2)
        validator: with1Decimal ? doubleVal : intVal
        text: Number.parseInt(defaultValue);
        afterLabel.text: showPercent ? "%" : ""
    }
}
