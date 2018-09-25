//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

import QtQuick 2.9
import QtQuick.Controls 2.2


DropArea {
    id: trashCan
    objectName: "DropTrash"
    property string __debugName: "DropTrash"

    keys: ["all"]

    onDropped: if (drop.drag.source !== null)
                   drop.drag.source.destroy()
    property real aspect: 1.3
    width: height / aspect
    property real iconPadding: 0.6

    property bool somethingHovers: false

    onEntered: somethingHovers = true
    onExited: somethingHovers = false

    Image {
        id: trashIcon
        anchors.centerIn: parent

        property real sizer: (trashCan.height < trashCan.width
                              * aspect ? trashCan.height : trashCan.width * aspect)

        height: (sizer) * parent.iconPadding
        width: (sizer) * parent.iconPadding

        // Flaticon basic license
        // Icon made by [Smashicons] (https://www.flaticon.com/authors/smashicons) from www.flaticon.com
        source: somethingHovers ? "qrc:/icons/trashcan.svg" : "qrc:/icons/trashcan_open.svg"
        sourceSize.width: 160 / aspect
        sourceSize.height: 160

        //mipmap: true
        smooth: true
    }

    MouseArea {
        anchors.fill: parent

        onDoubleClicked: parent.destroyAll()

        ToolTip.delay: 500
        //ToolTip.timeout: 1000
        ToolTip.visible: containsMouse
        ToolTip.text: "Dump unwanted formula snippets here.<br>Doubleclick to clean up the entire slate."

        hoverEnabled: true

        cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
    }

    function destroyAll() {
        for (var i = scriptColumn.children.length - 1; i >= 0; i--)
            scriptColumn.children[i].destroy()

        scriptColumn.children = ""
    }
} //! [0]
