import QtQuick

Item 
{
	function convertJSONtoFormulas(jsonString)
	{
		var jsonObj = JSON.parse(jsonString);

		if(jsonObj === null || jsonObj === undefined || jsonObj.formulas === undefined || jsonObj.formulas === null)
			return
		
		for(var i=0; i<jsonObj.formulas.length; i++)
			convertJSONtoItem(jsonObj.formulas[i], scriptColumn)
	}

	function convertJSONtoItem(jsonObj, dropItHere)
	{
		if(jsonObj === null || jsonObj === undefined)
			return

		var toolTip = jsonObj.toolTipText

		if(jsonObj.nodeType === "Operator" || jsonObj.nodeType === "OperatorVertical")
		{
			var operatorObj = (jsonObj.nodeType === "Operator" ? createOperator(jsonObj.operator, toolTip) : createOperatorVertical(jsonObj.operator, toolTip))
			operatorObj.releaseHere(dropItHere)

			convertJSONtoItem(jsonObj.leftArgument, operatorObj.leftDrop)
			convertJSONtoItem(jsonObj.rightArgument, operatorObj.rightDrop)
		}
		else if(jsonObj.nodeType === "Function")
		{
			var parameterNames = []
			var parameterDropKeys = []
			for(var i=0; i<jsonObj.arguments.length; i++)
			{
				parameterNames.push(jsonObj.arguments[i].name)
				parameterDropKeys.push(jsonObj.arguments[i].dropKeys)
			}

			var funcObj = createFunction(jsonObj.functionName, parameterNames, parameterDropKeys, toolTip)
			funcObj.releaseHere(dropItHere)

			for(var i=0; i<jsonObj.arguments.length; i++)
				convertJSONtoItem(jsonObj.arguments[i].argument, funcObj.getParameterDropSpot(jsonObj.arguments[i].name))
		}
		else if(jsonObj.nodeType === "RowFunction")
		{
			var rfuncObj = createRowFunction(jsonObj.functionName,  toolTip)
			rfuncObj.droppedItems = jsonObj.droppedItems
			
			rfuncObj.releaseHere(dropItHere)
		}
		else if(jsonObj.nodeType === "Number")
				createNumber(jsonObj.value, toolTip).releaseHere(dropItHere)
		else if(jsonObj.nodeType === "Boolean")
				createBoolean(jsonObj.value, toolTip).releaseHere(dropItHere)
		else if(jsonObj.nodeType === "String")
				createString(jsonObj.text, toolTip).releaseHere(dropItHere)
		else if(jsonObj.nodeType === "Column")
				createColumn(jsonObj.columnName, jsonObj.columnTypeUser, jsonObj.columnTypeDrop != undefined ? jsonObj.columnTypeDrop : -1, toolTip).releaseHere(dropItHere)
	}

	function createOperator(operator, toolTip)					{ return operatorComp.createObject(scriptColumn,		{ "toolTipText": toolTip, "operator": operator } ) }
	function createOperatorVertical(operator, toolTip)			{ return operatorvertComp.createObject(scriptColumn,	{ "toolTipText": toolTip, "operator": operator } ) }
	function createFunction(functionName, parameterNames,
							parameterDropKeys, toolTip)			{ return functionComp.createObject(scriptColumn,		{ "toolTipText": toolTip, "functionName": functionName,	"parameterNames": parameterNames, "parameterDropKeys": parameterDropKeys } ) }
	function createRowFunction(functionName,toolTip)			{ return rowFunctionComp.createObject(scriptColumn,		{ "toolTipText": toolTip, "functionName": functionName,	"droppedItems": ["null"] } ) }
	function createNumber(number, toolTip)						{ return numberComp.createObject(scriptColumn,			{ "toolTipText": toolTip, "value": number } ) }
	function createBoolean(value, toolTip)						{ return boolComp.createObject(scriptColumn,			{ "toolTipText": toolTip, "value": value } ) }
	function createString(text, toolTip)						{ return stringComp.createObject(scriptColumn,			{ "toolTipText": toolTip, "text": text } ) }
	function createColumn(columnName, columnTypeUser,
						  columnTypeDrop, toolTip)				{ return columnComp.createObject(scriptColumn,			{ "columnName": columnName,	"columnTypeUser": columnTypeUser, "columnTypeDrop": columnTypeDrop } ) }


	Component { id: operatorComp;		OperatorDrag			{ } }
	Component { id: operatorvertComp;	OperatorVerticalDrag	{ } }
	Component { id: functionComp;		FunctionDrag			{ } }
	Component { id: rowFunctionComp;	RowFunctionDrag			{ } }
	Component { id: numberComp;			NumberDrag				{ } }
	Component { id: boolComp;			BooleanDrag				{ } }
	Component { id: stringComp;			StringDrag				{ } }
	Component {	id: columnComp;			ColumnDrag				{ } }


}
