import QtQuick 2.0


ListView
{
	id:						listOfStuff
    implicitWidth:          150 * jaspTheme.uiScale
	spacing:				4
	maximumFlickVelocity:	jaspTheme.maximumFlickVelocity
	boundsBehavior:			Flickable.StopAtBounds


	property string	__debugName:	"ElementView"
	property real	maxWidth:		180 * preferencesModel.uiScale
	property real	widthMargin:	16 * preferencesModel.uiScale

	delegate: MouseArea
	{
	
		implicitWidth:  orientation === ListView.Horizontal ? elementLoader.item.width	: ListView.view.width
		implicitHeight: orientation === ListView.Horizontal ? ListView.view.height	: elementLoader.item.height

		z: 5

		property var alternativeDropFunctionDef: function(caller)
		{
			var obj = null

			//console.log("alternativeDropFunctionDef(",caller.__debugName,") type == ", type)

			var _operator = undefined, _functionName = undefined, _friendlyFunctionName = undefined, _parameterNames = undefined, _parameterDropKeys = undefined, _value = undefined, _text = undefined, _columnName = undefined, _columnTypeUser = undefined;

			if(type == "operator")			{ _operator = operator }
			else if(type == "operatorvert")	{ _operator = operator }
			else if(type == "function")
			{
				_functionName			= functionName;
				_friendlyFunctionName	= friendlyFunctionName != "" ? friendlyFunctionName : functionName;
				_parameterNames			= functionParameters.split(",");
				_parameterDropKeys		= functionParamTypes.split(",");

				for(var param=0; param<_parameterDropKeys.length; param++)
					_parameterDropKeys[param] = _parameterDropKeys[param].split(":");

			}
			else if(type == "number")		{ _value = number }
			else if(type == "string")		{ _text = text }
			else if(type == "column")		{ _columnName = columnName; }

			if(type == "operator")			obj = operatorCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip,	"alternativeDropFunction": null, "operator": _operator,			"acceptsDrops": true})
			else if(type == "operatorvert")	obj = operatorvertCompBetterContext.createObject(scriptColumn,	{ "toolTipText": toolTip,	"alternativeDropFunction": null, "operator": _operator,			"acceptsDrops": true})
			else if(type == "function")		obj = functionCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip,	"alternativeDropFunction": null, "functionName": _functionName,	"acceptsDrops": true, "parameterNames": _parameterNames, "parameterDropKeys": _parameterDropKeys, "friendlyFunctionName": _friendlyFunctionName })
			else if(type == "number")		obj = numberCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip,	"alternativeDropFunction": null, "value": _value,				"acceptsDrops": true})
			else if(type == "string")		obj = stringCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip,	"alternativeDropFunction": null, "text": _text,					"acceptsDrops": true})
			else if(type == "column")		obj = columnCompBetterContext.createObject(scriptColumn,		{							"alternativeDropFunction": null, "columnName": _columnName,		"acceptsDrops": true,	"columnTypeUser": -1 })

			return obj
		}

		Loader
		{
			id: elementLoader

			property bool isColumn:				type === "column"
			property bool isOperator:			type !== undefined && type.indexOf("operator") >=0
			property string listOperator:		isOperator			?	operator			: "???"
			property string listFunction:		type !== "function"	?	"???"				: friendlyFunctionName != "" ? friendlyFunctionName : functionName
			property real	listNumber:			type === "number"	?	number				: -1
			property string	listText:			type === "string"	?	text				: "???"
			property real	listWidth:			parent.width
			property string	listColName:		isColumn			?	columnName			: "???"
			property string listToolTip:		type !== "separator" && type !== "text" && toolTip !== undefined? toolTip : ""

			//anchors.centerIn: parent
			x: isColumn ? listOfStuff.widthMargin / 2 : (parent.width - width) - (listOfStuff.widthMargin / 2)

			sourceComponent: type === undefined ?
								defaultComp :
								type === "operator" ?
									 operatorComp :
									 type === "operatorvert" ?
										 operatorvertComp :
										 type === "function" ?
											 functionComp :
											 type === "number" ?
												 numberComp :
												 type === "string" ?
													 stringComp :
													 type === "column" ?
														 columnComp :
														 type === "separator" ?
															 separatorComp :
															 defaultComp

		}

		onDoubleClicked: alternativeDropFunctionDef()

		Component { id: operatorComp;		OperatorDrag			{ toolTipText: listToolTip; operator: listOperator;		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: operatorvertComp;	OperatorVerticalDrag	{ toolTipText: listToolTip; operator: listOperator;		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: functionComp;		FunctionDrag			{ toolTipText: listToolTip; functionName: listFunction;	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: numberComp;			NumberDrag				{ toolTipText: listToolTip; value: listNumber;									alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: stringComp;			StringDrag				{ toolTipText: listToolTip; text: listText;										alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: separatorComp;		Item					{ height: filterConstructor.blockDim; width: listWidth - listOfStuff.widthMargin; Rectangle { height: 1; color: jaspTheme.black; width: parent.width ; anchors.centerIn: parent }  } }
		Component { id: defaultComp;		Text					{ text: "Something wrong!"; color: jaspTheme.red }  }
		Component {	id: columnComp;			ColumnDrag				{ columnName: listColName;	columnTypeUser:	-1;			acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef; maxSize: listOfStuff.maxWidth } }
	}


	Component { id: operatorCompBetterContext;		OperatorDrag			{ } }
	Component { id: operatorvertCompBetterContext;	OperatorVerticalDrag	{ } }
	Component { id: functionCompBetterContext;		FunctionDrag			{ } }
	Component { id: numberCompBetterContext;		NumberDrag				{ } }
	Component { id: stringCompBetterContext;		StringDrag				{ } }
	Component { id: separatorCompBetterContext;		Item					{ } }
	Component { id: defaultCompBetterContext;		Text					{ } }
	Component {	id: columnCompBetterContext;		ColumnDrag				{ } }
}
