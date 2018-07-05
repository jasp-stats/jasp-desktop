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
    Component {
        id: operatorvertComp
        OperatorVerticalDrag {
        }
    }
    Component {
        id: functionComp
        FunctionDrag {
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
        else if (caller.shownChild.objectName === "OperatorVertical")
            obj = operatorvertComp.createObject(scriptColumn, {
                                                    toolTipText: caller.toolTipText,
                                                    alternativeDropFunction: null,
                                                    operator: caller.operator,
                                                    acceptsDrops: true
                                                })
        else if (caller.shownChild.objectName === "Function")
            obj = functionComp.createObject(scriptColumn, {
                                                toolTipText: caller.toolTipText,
                                                alternativeDropFunction: null,
                                                functionName: caller.functionName,
                                                acceptsDrops: true
                                            }) //,  "parameterNames": caller.functionName === "!" ? ["logical(s)"] : ["value(s)"], parameterDropKeys: caller.functionName === "!" ? ["boolean"] : ["number"] })

        return obj
    }
}
