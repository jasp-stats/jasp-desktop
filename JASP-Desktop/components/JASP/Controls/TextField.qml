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

<<<<<<< HEAD
import QtQuick 2.10
import QtQuick.Controls 2.3
=======
import QtQuick 2.11
import QtQuick.Controls 2.4
>>>>>>> qmlFormsB
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: textField
    controlType: "TextField"
<<<<<<< HEAD
    implicitHeight: control.height
    implicitWidth: control.width + (beforeLabel.visible ? (labelSpacing + beforeLabel.implicitWidth) : 0) + (afterLabel.visible ? (labelSpacing + afterLabel.implicitWidth) : 0)
    property alias text: control.text
=======
    implicitHeight: row.implicitHeight
    implicitWidth: row.implicitWidth
    controlBackground: useExternalBorder ? externalControlBackround : controlBackground
    property alias text: control.text
    property alias value: control.text
    property int textWidth: Theme.textFieldWidth
    property int textHeight: Theme.textFieldHeight
    property bool useExternalBorder: true
    property alias placeholderText: control.placeholderText
>>>>>>> qmlFormsB
    property alias validator: control.validator
    property alias control: control
    property alias label: beforeLabel
    property alias beforeLabel: beforeLabel
    property alias afterLabel: afterLabel
    property string inputType: "string"
    property int labelSpacing: 4
    signal editingFinished()
<<<<<<< HEAD
    
    Component.onCompleted: {
        control.editingFinished.connect(editingFinished);
        control.background.color = "white"
    }
    
    RowLayout {
        spacing: labelSpacing
        anchors.fill: parent
        Label {
            id: beforeLabel
            visible: beforeLabel.text && textField.visible ? true : false
        }    
        TextField {
            id: control
            focus: true
            width: 40
            height: 20
            padding: 1
            rightPadding: 5
            background: backgroundRectangle
        }
=======
    signal textEdited()
    signal pressed()
    signal released()
    
    Component.onCompleted: {
        control.editingFinished.connect(editingFinished);
        control.textEdited.connect(textEdited);
        control.pressed.connect(pressed);
        control.released.connect(released);
    }
    
    RowLayout {
        id: row
        spacing: labelSpacing
        Label {
            id: beforeLabel
            visible: beforeLabel.text && textField.visible ? true : false
        }
        
        TextField {
            id: control
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: textField.textWidth
            Layout.preferredHeight: textField.textHeight
            focus: true
            padding: 1
            rightPadding: 5
            selectByMouse: true
            background: Rectangle {
                id: controlBackground
                color: Theme.controlBackgroundColor
                border.width: textField.useExternalBorder && !control.activeFocus ? 1 : 0
                border.color: textField.useExternalBorder ? Theme.borderColor : "transparent"
            }
            
            Rectangle {
                id: externalControlBackround
                height: textField.textHeight + 6
                width: textField.textWidth + 6
                color: "transparent"
                border.width: 1
                border.color: "transparent"
                anchors.centerIn: parent
                visible: textField.useExternalBorder
            }
        }
            
>>>>>>> qmlFormsB
        Label {
            id: afterLabel
            visible: afterLabel.text && textField.visible ? true : false
        }
    }
}
