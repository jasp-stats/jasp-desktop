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

		OperatorDrag			{ operator: "+";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Addition" }
		OperatorDrag			{ operator: "-";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Subtraction" }
		OperatorDrag			{ operator: "*";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Multiplication" }
		OperatorVerticalDrag	{ operator: "/";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Division" }
		OperatorDrag			{ operator: "/";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Division" }
		OperatorDrag			{ operator: "^";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Power (2^3 returns 8)" }
		FunctionDrag			{ functionName: "sqrt";	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; parameterNames: []; parameterDropKeys: []; toolTipText: "Square root" }
		OperatorDrag			{ operator: "%%";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Modulo: returns the remainder of a division. 3%2 returns 1" }
		property string logicalnessText: ": returns logicals"
		OperatorDrag			{ operator: "==";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Equality" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "!=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Inequality" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "<";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Less than" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "<=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Less than or equal to" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: ">";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Greater than" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: ">=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Greater than or equal to" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "&";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "And" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "|";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Or" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "%|%";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "Split: applies filter separately to each subgroup" }
		FunctionDrag			{ functionName: "!";	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; parameterNames: []; parameterDropKeys: []; toolTipText: "not" + operatorRow.logicalnessText }

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
