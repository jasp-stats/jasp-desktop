import QtQuick 2.0

Item
{
	//color: filterConstructor.jaspDarkBlue
	property string __debugName: "OperatorSelector"
	z: 2
	property real horizontalCenterX: width / 2
	property real verticalCenterY: height / 2

	Row
	{
		id: operatorRow
		x: parent.horizontalCenterX - (width / 2)
		y: parent.verticalCenterY - (height / 2)

		OperatorDrag			{ operator: "+";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Addition") }
		OperatorDrag			{ operator: "-";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Subtraction") }
		OperatorDrag			{ operator: "*";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Multiplication") }
		OperatorVerticalDrag	{ operator: "/";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Division") }
		OperatorDrag			{ operator: "/";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Division") }
		OperatorDrag			{ operator: "^";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Power (2^3 returns 8)") }
		FunctionDrag			{ functionName: "sqrt";	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; parameterNames: []; parameterDropKeys: []; toolTipText: qsTr("Square root") }
		OperatorDrag			{ operator: "%%";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Modulo: returns the remainder of a division. 3%2 returns 1") }
		property string logicalnessText: qsTr("returns logicals")
		OperatorDrag			{ operator: "==";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Equality: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: "!=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Inequality: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: "<";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Less than: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: "<=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Less than or equal to: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: ">";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Greater than: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: ">=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Greater than or equal to: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: "&";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("And: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: "|";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Or: %1").arg(operatorRow.logicalnessText) }
		OperatorDrag			{ operator: "%|%";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: qsTr("Split: applies filter separately to each subgroup") }
		FunctionDrag			{ functionName: "!";	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; parameterNames: []; parameterDropKeys: []; toolTipText: qsTr("Not: %1").arg(operatorRow.logicalnessText) }

	}


	Component { id: operatorComp;		OperatorDrag			{ } }
	Component { id: operatorvertComp;	OperatorVerticalDrag	{ } }
	Component { id: functionComp;		FunctionDrag			{ } }

	property var alternativeDropFunctionDef: function(caller)
	{
		var obj = null

		if(caller.shownChild.objectName === "Operator")					obj = operatorComp.createObject(scriptColumn,		{ "toolTipText": caller.toolTipText, "alternativeDropFunction": null, "operator": caller.operator,			"acceptsDrops": true})
		else if(caller.shownChild.objectName === "OperatorVertical")	obj = operatorvertComp.createObject(scriptColumn,	{ "toolTipText": caller.toolTipText, "alternativeDropFunction": null, "operator": caller.operator,			"acceptsDrops": true})
		else if(caller.shownChild.objectName === "Function")			obj = functionComp.createObject(scriptColumn,		{ "toolTipText": caller.toolTipText, "alternativeDropFunction": null, "functionName": caller.functionName,	"acceptsDrops": true,  "parameterNames": caller.functionName === "!" ? ["logical(s)"] : ["value(s)"], parameterDropKeys: caller.functionName === "!" ? ["boolean"] : ["number"] })

		return obj
	}

}
