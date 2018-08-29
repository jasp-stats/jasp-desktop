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
import QtQuick.Controls 2.4
import JASP.Theme 1.0

JASPControl {
    controlType: "RadioButton"
    isBound: false
    useDefaultBackground: true
    property alias text: control.text
    property alias checked: control.checked
    implicitHeight: control.height
    implicitWidth: control.width

    property var buttonGroup

    RadioButton {
        id: control
        height: Theme.radioIndicatorDiameter + 4
        width: Theme.radioIndicatorDiameter + label.implicitWidth + control.spacing + 6
        focus: true
        ButtonGroup.group: buttonGroup

        indicator: Rectangle {
            width: Theme.radioIndicatorDiameter
            height: Theme.radioIndicatorDiameter
            anchors.left: control.left
            anchors.leftMargin: 2
            anchors.top: control.top
            anchors.topMargin: 2
            radius: Theme.radioIndicatorDiameter / 2
            color: control.checked ? (control.enabled ? Theme.buttonBackgroundColor : Theme.disableControlBackgroundColor) : Theme.controlBackgroundColor
            border.color: control.enabled ? (control.checked ? Theme.buttonBackgroundColor : Theme.borderColor) : Theme.disableControlBackgroundColor
            border.width: 1

            Rectangle {
                anchors.centerIn: parent
                implicitWidth: Theme.radioIndicatorDiameter / 2
                implicitHeight: Theme.radioIndicatorDiameter / 2
                radius: Theme.radioIndicatorDiameter / 4
                visible: control.checked
                color: Theme.controlBackgroundColor
            }
        }

        contentItem: Label {
            id: label
            anchors.left: control.indicator.right
            anchors.leftMargin: control.spacing
            anchors.top: control.top
            anchors.topMargin: 2
            text: control.text
        }

    }
}
