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


Item {
    property string __debugName: "OperatorSelector"
    z: 2
    property real horizontalCenterX: width / 2
    property real verticalCenterY: height / 2

    Row {
        id: operatorRow
        x: parent.horizontalCenterX - (width / 2)
        y: parent.verticalCenterY - (height / 2)

        // property string logicalnessText: ": returns logicals and can be the root of a filter formula"
        OperatorDrag {
            operator: "=="
            acceptsDrops: false
            alternativeDropFunction: alternativeDropFunctionDef
            toolTipText: "Equality"
        }
        OperatorDrag {
            operator: "<"
            acceptsDrops: false
            alternativeDropFunction: alternativeDropFunctionDef
            toolTipText: "Less than"
        }
        OperatorDrag {
            operator: ">"
            acceptsDrops: false
            alternativeDropFunction: alternativeDropFunctionDef
            toolTipText: "Greater than"
        }
    }

    Component {
        id: operatorComp
        OperatorDrag {
        }
    }

    property var alternativeDropFunctionDef: function (caller) {
        var obj = null

        if (caller.shownChild.objectName === "Operator")
            obj = operatorComp.createObject(scriptColumn, {
                                                toolTipText: caller.toolTipText,
                                                alternativeDropFunction: null,
                                                operator: caller.operator,
                                                acceptsDrops: true
                                            })

        return obj
    }
}
