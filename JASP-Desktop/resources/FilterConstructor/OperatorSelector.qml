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

		OperatorDrag			{ operator: "+";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "addition" }
		OperatorDrag			{ operator: "-";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "subtraction" }
		OperatorDrag			{ operator: "*";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "multiplication" }
		OperatorVerticalDrag	{ operator: "/";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "division" }
		OperatorDrag			{ operator: "/";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "division" }
		OperatorDrag			{ operator: "^";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "power, 2^3 returns 8" }
		FunctionDrag			{ functionName: "sqrt";	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; parameterNames: []; parameterDropKeys: []; toolTipText: "square root" }
		OperatorDrag			{ operator: "%%";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "modulo, returns the remainder of a division. 3%2 returns 1" }
		property string logicalnessText: ", returns a logical and can be used as the root of a formula for filtering"
		OperatorDrag			{ operator: "==";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "equality" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "!=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "inequality" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "<";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "less than" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "<=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "less than or equal to" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: ">";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "greater than" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: ">=";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "greater than or equal to" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "&";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "and" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "|";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "or" + operatorRow.logicalnessText }
		OperatorDrag			{ operator: "%|%";		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; toolTipText: "condition by, divides all results and data on the left-hand side up according to the categories on its right" }
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
