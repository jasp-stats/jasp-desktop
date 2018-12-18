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
import QtGraphicalEffects 1.0
import JASP.Theme 1.0

Rectangle {
    id                            : ribbonButton
    width                         : (innerText.width > backgroundImage.width ? innerText.width : backgroundImage.width) + 20 // + 2*tbutton.width
    height                        : 60  // backgroundImage.height + innerText.height
    radius                        : 5
    color                         : "#FFFFFF"
            property alias text   : innerText.text
            property alias source : backgroundImage.source
            property alias enabled: mice.enabled
    default property var   menu

    signal clicked

    Image {
        width: 37
        height: 28
        id: backgroundImage

        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.topMargin: 5
    }

    ColorOverlay {
        anchors.fill: backgroundImage
        source      : backgroundImage
        color       : mice.enabled ? "transparent" : "lightgrey"
    }

    Text {
        id: innerText

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.top             : backgroundImage.bottom
        anchors.bottom          : parent.bottom
        anchors.topMargin       : 5

        color    : mice.enabled ? Theme.black : "lightgrey"
        font.bold: false
    }

    MouseArea {
        id          : mice
        anchors.fill: parent
        hoverEnabled: true
        acceptedButtons: Qt.LeftButton
        onClicked   : {
            clusterMenu.popup()
        }
        onPressed   : ribbonButton.color = Qt.tint("grey", "#EEEEEE")
        onReleased  : ribbonButton.color = "transparent"
        onEntered   : ribbonButton.border.color = Qt.tint("grey", "#10FF0000")
        onExited    : {
            ribbonButton.border.color = "transparent";
            ribbonButton.color        = "#FFFFFF";
        }

        ClusterMenu {
            id: clusterMenu
        }
    }
}
