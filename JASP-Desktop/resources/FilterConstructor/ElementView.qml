import QtQuick 2.0

ListView {
	id: listOfStuff

	//clip: true



	delegate: MouseArea
	{

		width:  orientation === ListView.Horizontal ? elementLoader.width	: ListView.view.width
		height: orientation === ListView.Horizontal ? ListView.view.height	: elementLoader.height

		z: 5

		property var alternativeDropFunctionDef: function(caller)
		{
			var obj = null

			if(type == "operator")			obj = operatorComp.createObject(scriptColumn,		{ "toolTipText": toolTip, "alternativeDropFunction": null, "operator": operator,			"acceptsDrops": true})
			else if(type == "operatorvert")	obj = operatorvertComp.createObject(scriptColumn,	{ "toolTipText": toolTip, "alternativeDropFunction": null, "operator": operator,			"acceptsDrops": true})
			else if(type == "function")		obj = functionComp.createObject(scriptColumn,		{ "toolTipText": toolTip, "alternativeDropFunction": null, "functionName": functionName,	"acceptsDrops": true, "parameterNames": functionParameters.split(","), "parameterDropKeys": functionParamTypes.split(",") })
			else if(type == "number")		obj = numberComp.createObject(scriptColumn,			{ "toolTipText": toolTip, "alternativeDropFunction": null, "value": number,					"acceptsDrops": true})
			else if(type == "string")		obj = stringComp.createObject(scriptColumn,			{ "toolTipText": toolTip, "alternativeDropFunction": null, "text": text,					"acceptsDrops": true})
			else if(type == "column")		obj = columnComp.createObject(scriptColumn,			{ "toolTipText": toolTip, "alternativeDropFunction": null, "columnName": columnName,		"acceptsDrops": true,	"columnIcon": columnIcon})

			return obj
		}

		Loader
		{
			id: elementLoader

			property bool isColumn:				type === "column"
			property bool isOperator:			type.indexOf("operator") >=0
			property string listOperator:		isOperator			?	operator			: "???"
			property string listFunction:		type === "function"	?	functionName		: "???"
			property real	listNumber:			type === "number"	?	number				: -1
			property string	listText:			type === "string"	?	text				: "???"
			property real	listWidth:			parent.width
			property string	listColName:		isColumn			?	columnName			: "???"
			property string	listColIcon:		isColumn			?	columnIcon			: "???"
			property string listToolTip:		type !== "separator" && type !== "text" ? toolTip : ""

			//anchors.centerIn: parent
			x: isColumn ? 0 : (parent.width - width)

			sourceComponent: type === "operator" ?
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

			onLoaded:
			{

				if(listOfStuff.orientation !== ListView.Horizontal && listOfStuff.width < width)
					listOfStuff.width = width
			}
		}

		onDoubleClicked: alternativeDropFunctionDef()

		Component { id: operatorComp;		OperatorDrag			{ toolTipText: listToolTip; operator: listOperator;		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: operatorvertComp;	OperatorVerticalDrag	{ toolTipText: listToolTip; operator: listOperator;		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: functionComp;		FunctionDrag			{ toolTipText: listToolTip; functionName: listFunction;	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: numberComp;			NumberDrag				{ toolTipText: listToolTip; value: listNumber;									alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: stringComp;			StringDrag				{ toolTipText: listToolTip; text: listText;										alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: separatorComp;		Item					{ height: filterConstructor.blockDim; width: listWidth; Rectangle { height: 1; color: "black"; width: listWidth ; anchors.centerIn: parent }  } }
		Component { id: defaultComp;		Text					{ text: "Something wrong!"; color: "red" }  }
		Component {	id: columnComp;			ColumnDrag				{ toolTipText: listToolTip; columnName: listColName; columnIcon: listColIcon;		alternativeDropFunction: alternativeDropFunctionDef } }



	}
}
