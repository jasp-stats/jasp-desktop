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

Rectangle {
    id: control
    
    property int leftPadding: Theme.groupContentPadding
    property int spacing: Theme.rowSpacing
    property string title: ""
    property bool debug: false
    property var childControls: []
    default property alias content: column.children
    implicitHeight: (title ? 20 : 0) + column.childrenRect.height
    implicitWidth: column.childrenRect.width + (title ? control.leftPadding : 0)
    
    color: Theme.analysisBackgroundColor // transparent generates sometimes temporary black blocks
    
    Label {
        id: label
        anchors.top: control.top
        anchors.left: control.left
        text: control.title
        visible: control.title ? true : false
    }
    
    ColumnLayout {
        id: column
        anchors.top: control.title ? label.bottom : control.top
        anchors.left: control.left
        anchors.leftMargin: control.title ? control.leftPadding : 0
        spacing: control.spacing
    }
    
    Component.onCompleted: {
        form.getJASPControls(childControls, column)
        for (var i = 0; i < childControls.length; i++) {
            if (control.debug)
                childControls[i].debug = true;
        }
    }
    
}
