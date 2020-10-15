import QtQuick 2.0


ListView
{
	id:						listOfStuff
	width:					0
	spacing:				4
	maximumFlickVelocity:	jaspTheme.maximumFlickVelocity
	boundsBehavior:			Flickable.StopAtBounds


	property string	__debugName:	"ElementView"
	property real	maxWidth:		300 * preferencesModel.uiScale
	property real	widthMargin:	10

	property int	_recalculateWidth: 0 //To trigger a recalculation of the width from the delegates

	onMaxWidthChanged:
	{
		listOfStuff.width = 0;
		_recalculateWidth++;
	}

	delegate: MouseArea
	{

		width:  orientation === ListView.Horizontal ? elementLoader.width	: ListView.view.width
		height: orientation === ListView.Horizontal ? ListView.view.height	: elementLoader.height

		z: 5

		property var alternativeDropFunctionDef: function(caller)
		{
			var obj = null

			//console.log("alternativeDropFunctionDef(",caller.__debugName,") type == ", type)

			var _operator = undefined, _functionName = undefined, _parameterNames = undefined, _parameterDropKeys = undefined, _value = undefined, _text = undefined, _columnName = undefined, _columnIcon = undefined;

			if(type == "operator")			{ _operator = operator }
			else if(type == "operatorvert")	{ _operator = operator }
			else if(type == "function")
			{
				_functionName		= functionName;
				_parameterNames		= functionParameters.split(",");
				_parameterDropKeys	= functionParamTypes.split(",");

				for(var param=0; param<_parameterDropKeys.length; param++)
					_parameterDropKeys[param] = _parameterDropKeys[param].split(":");

			}
			else if(type == "number")		{ _value = number }
			else if(type == "string")		{ _text = text }
			else if(type == "column")		{ _columnName = columnName; _columnIcon = columnIcon }

			if(type == "operator")			obj = operatorCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip, "alternativeDropFunction": null, "operator": _operator,			"acceptsDrops": true})
			else if(type == "operatorvert")	obj = operatorvertCompBetterContext.createObject(scriptColumn,	{ "toolTipText": toolTip, "alternativeDropFunction": null, "operator": _operator,			"acceptsDrops": true})
			else if(type == "function")		obj = functionCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip, "alternativeDropFunction": null, "functionName": _functionName,	"acceptsDrops": true, "parameterNames": _parameterNames, "parameterDropKeys": _parameterDropKeys })
			else if(type == "number")		obj = numberCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip, "alternativeDropFunction": null, "value": _value,					"acceptsDrops": true})
			else if(type == "string")		obj = stringCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip, "alternativeDropFunction": null, "text": _text,					"acceptsDrops": true})
			else if(type == "column")		obj = columnCompBetterContext.createObject(scriptColumn,		{ "toolTipText": toolTip, "alternativeDropFunction": null, "columnName": _columnName,		"acceptsDrops": true,	"columnIcon": columnIcon})

			return obj
		}

		Loader
		{
			id: elementLoader

			property bool isColumn:				type === "column"
			property bool isOperator:			type !== undefined && type.indexOf("operator") >=0
			property string listOperator:		isOperator			?	operator			: "???"
			property string listFunction:		type === "function"	?	functionName		: "???"
			property real	listNumber:			type === "number"	?	number				: -1
			property string	listText:			type === "string"	?	text				: "???"
			property real	listWidth:			parent.width
			property string	listColName:		isColumn			?	columnName			: "???"
			property string	listColIcon:		isColumn			?	columnIcon			: "???"
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

			function calcWidth()
			{
				if(listOfStuff.orientation !== ListView.Horizontal && listOfStuff.width < width + listOfStuff.widthMargin)
					listOfStuff.width = width + listOfStuff.widthMargin

			}

			property int _recalculateWidth: 0

			onLoaded:
			{
				_recalculateWidth = Qt.binding(function() { return listOfStuff._recalculateWidth; } )

				calcWidth()
			}

			on_RecalculateWidthChanged: calcWidth()
			onWidthChanged: calcWidth()
		}

		onDoubleClicked: alternativeDropFunctionDef()

		Component { id: operatorComp;		OperatorDrag			{ toolTipText: listToolTip; operator: listOperator;		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: operatorvertComp;	OperatorVerticalDrag	{ toolTipText: listToolTip; operator: listOperator;		acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: functionComp;		FunctionDrag			{ toolTipText: listToolTip; functionName: listFunction;	acceptsDrops: false;	alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: numberComp;			NumberDrag				{ toolTipText: listToolTip; value: listNumber;									alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: stringComp;			StringDrag				{ toolTipText: listToolTip; text: listText;										alternativeDropFunction: alternativeDropFunctionDef } }
		Component { id: separatorComp;		Item					{ height: filterConstructor.blockDim; width: listWidth - listOfStuff.widthMargin; Rectangle { height: 1; color: jaspTheme.black; width: parent.width ; anchors.centerIn: parent }  } }
		Component { id: defaultComp;		Text					{ text: "Something wrong!"; color: jaspTheme.red }  }
		Component {	id: columnComp;			ColumnDrag				{ toolTipText: listToolTip; columnName: listColName; columnIcon: listColIcon;		alternativeDropFunction: alternativeDropFunctionDef } }
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
