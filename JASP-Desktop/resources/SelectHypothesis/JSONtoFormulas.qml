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
    function convertJSONtoFormulas(jsonObj) {
        if (jsonObj === null || jsonObj === undefined)
            return

        for (var i = 0; i < jsonObj.formulas.length; i++)
            convertJSONtoItem(jsonObj.formulas[i], scriptColumn)
    }

    function convertJSONtoItem(jsonObj, dropItHere) {
        if (jsonObj === null || jsonObj === undefined)
            return

        var toolTip = jsonObj.toolTipText

        if (jsonObj.nodeType === "Operator") {
            var operatorObj = createOperator(jsonObj.operator, toolTip)
            operatorObj.releaseHere(dropItHere)

            convertJSONtoItem(jsonObj.leftArgument, operatorObj.leftDrop)
            convertJSONtoItem(jsonObj.rightArgument, operatorObj.rightDrop)

        } else if (jsonObj.nodeType === "Column")
            createColumn(jsonObj.columnName, toolTip).releaseHere(dropItHere)
    }

    function createOperator(operator, toolTip) {
        return operatorComp.createObject(scriptColumn, {
                                             toolTipText: toolTip,
                                             operator: operator
                                         })
    }

    function createColumn(columnName, toolTip) {
        return columnComp.createObject(scriptColumn, { toolTipText: toolTip })
    }

    Component {
        id: operatorComp
        OperatorDrag { }
    }

    Component {
        id: columnComp
        ColumnDrag { }
    }
}
