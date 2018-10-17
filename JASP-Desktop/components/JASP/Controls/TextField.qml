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
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: textField
    controlType: "TextField"
    implicitHeight: row.implicitHeight
    implicitWidth: row.implicitWidth
    controlBackground: showBorder ? backgroundTextField : controlBackground
    property alias text: control.text
    property alias value: control.text
    property int textWidth: Theme.textFieldWidth
    property int textHeight: Theme.textFieldHeight
    property bool showBorder: true
    property alias placeholderText: control.placeholderText
    property alias validator: control.validator
    property alias control: control
    property alias label: beforeLabel
    property alias beforeLabel: beforeLabel
    property alias afterLabel: afterLabel
    property string inputType: "string"
    property int labelSpacing: 4
    signal editingFinished()
    signal textEdited()
    
    Component.onCompleted: {
        control.editingFinished.connect(editingFinished);
        control.textEdited.connect(textEdited);
    }
    
    RowLayout {
        id: row
        spacing: labelSpacing
        Label {
            id: beforeLabel
            visible: beforeLabel.text && textField.visible ? true : false
        }
        Rectangle {
            id: backgroundTextField
            height: textField.textHeight + 6
            width: textField.textWidth + 6
            color: "transparent"
            border.width: 1
            border.color: "transparent"
            TextField {
                id: control
                focus: true
                anchors.margins: 3
                anchors.top: parent.top
                anchors.left: parent.left                
                height: textField.textHeight
                width: textField.textWidth
                padding: 1
                rightPadding: 5
                selectByMouse: true
                background: Rectangle {
                    id: controlBackground
                    anchors.fill: parent
                    color: Theme.controlBackgroundColor
                    border.width: textField.showBorder ? 1 : 0
                    border.color: backgroundTextField.border.width === 1 ? Theme.borderColor : "transparent"
                }
            }
            
        }
        Label {
            id: afterLabel
            visible: afterLabel.text && textField.visible ? true : false
        }
    }
}
