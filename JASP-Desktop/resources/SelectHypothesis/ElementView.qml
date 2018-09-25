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

import QtQuick 2.0


ListView {
    id                         : listOfStuff
    property string __debugName: "ElementView"
    property real   maxWidth   : 200
    property real   widthMargin: 10
    spacing                    : 4

    delegate: MouseArea {

        width : orientation === ListView.Horizontal ? elementLoader.width  : ListView.view.width
        height: orientation === ListView.Horizontal ? ListView.view.height : elementLoader.height
        z     : 5

        property var alternativeDropFunctionDef: function (caller) {
            if (type != "column") { return null }

            var details = {
                toolTipText            : "",
                alternativeDropFunction: null,
                columnName             : columnName,
                acceptsDrops           : true
            }

            return columnCompBetterContext.createObject(scriptColumn, details)
        }

        Loader {
            id: elementLoader

            property bool   isColumn   : type === "column"
            property real   listWidth  : parent.width
            property string listColName: isColumn ? columnName : "???"
            property string listToolTip: ""

            //anchors.centerIn: parent
            x: isColumn ? listOfStuff.widthMargin / 2 : (parent.width - width) - (listOfStuff.widthMargin / 2)

            sourceComponent: type === "column" ? columnComp : defaultComp

            onLoaded: {
                if (listOfStuff.orientation !== ListView.Horizontal && listOfStuff.width < width + listOfStuff.widthMargin)
                    listOfStuff.width = width + listOfStuff.widthMargin
            }
        }

        onDoubleClicked: alternativeDropFunctionDef()

        Component {
            id: defaultComp
            Text {
                text : "Something wrong!"
                color: "red"
            }
        }

        Component {
            id: columnComp
            ColumnDrag {
                toolTipText            : listToolTip
                columnName             : listColName
                alternativeDropFunction: alternativeDropFunctionDef
            }
        }
    }

    Component {
        id: defaultCompBetterContext
        Text { }
    }

    Component {
        id: columnCompBetterContext
        ColumnDrag { }
    }
}
