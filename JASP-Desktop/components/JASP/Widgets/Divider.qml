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


import QtQuick 2.0
import QtQuick.Layouts 1.3
import JASP.Theme 1.0


ColumnLayout {
    spacing: 5
	Layout.columnSpan: parent.columns
    implicitHeight: 2
    implicitWidth: parent.width
    property alias label: textLabel.text

    Rectangle {
        border.width: 1
        implicitHeight: parent.implicitHeight
        implicitWidth: parent.implicitWidth
        border.color: Theme.black
    }
    Text {
        id: textLabel
        visible: text !== ''
        horizontalAlignment: Text.AlignHCenter
        font.bold: true
    }
}
