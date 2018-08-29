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
    implicitHeight: control.height
    implicitWidth: control.implicitWidth + (beforeLabel.visible ? (labelSpacing + beforeLabel.implicitWidth) : 0) + (afterLabel.visible ? (labelSpacing + afterLabel.implicitWidth) : 0)
    useDefaultBackground: true
    property alias text: control.text
    property alias placeholderText: control.placeholderText
    property alias validator: control.validator
    property alias control: control
    property alias label: beforeLabel
    property alias beforeLabel: beforeLabel
    property alias afterLabel: afterLabel
    property string inputType: "string"
    property int labelSpacing: 4
    signal editingFinished()    
    
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
            height: Theme.textFieldHeight
            implicitWidth: Theme.textFieldWidth
            padding: 1
            rightPadding: 5
            selectByMouse: true
        }
        Label {
            id: afterLabel
            visible: afterLabel.text && textField.visible ? true : false
        }
    }
}
