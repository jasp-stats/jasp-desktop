import QtQuick 2.0

DragGeneric {
	shownChild: showMe

	property string __debugName: "OperatorDrag"

	readonly property var	everythingOperators:	["==", "!="]
	readonly property var	numberCompareOperators: ["<", ">", "<=", ">="]
	readonly property var	booleanOperators:		["&", "|"]
	readonly property bool	acceptsBoolean:			booleanOperators.indexOf(operator)			>= 0
	readonly property bool	acceptsEverything:		everythingOperators.indexOf(operator)		>= 0
	readonly property bool	isNumberComparer:		numberCompareOperators.indexOf(operator)	>= 0
	readonly property bool	isConditionalOp:		operator == "%|%"

	readonly property bool amInsideColumnConstructor: filterConstructor.isColumnConstructor

	readonly property bool returnsBoolean: booleanOperators.indexOf(operator) >= 0 || numberCompareOperators.indexOf(operator) >= 0 || acceptsEverything || ( isConditionalOp && !amInsideColumnConstructor)


	dragKeys:		returnsBoolean ? ["boolean"] : ["number"]
	leftDropSpot:	showMe.leftDropSpot

	property string operator: "+"
	property bool acceptsDrops: true

	property var opImages: { '==': 'qrc:/icons/equal.png', '!=': 'qrc:/icons/notEqual.png', '<': 'qrc:/icons/lessThan.png', '>': 'qrc:/icons/greaterThan.png', '<=': 'qrc:/icons/lessThanEqual.png', '>=': 'qrc:/icons/greaterThanEqual.png',  '&': 'qrc:/icons/and.png', '|': 'qrc:/icons/or.png', '%%': 'qrc:/icons/modulo.png', '%|%': 'qrc:/icons/ConditionBy.png'}


	property alias leftDrop: showMe.leftDrop
	property alias rightDrop: showMe.rightDrop

	Operator
	{
		id:							showMe
		operator:					parent.operator
		operatorImageSource:		parent.opImages[operator] !== null && parent.opImages[operator] !== undefined ? parent.opImages[operator] : ""

		dropKeysLeft:				isNumberComparer ? ["number", "ordered"] : isConditionalOp ? (amInsideColumnConstructor ? ["number"] : ["boolean"]) : acceptsEverything ? ["boolean", "string", "number"] : acceptsBoolean ? ["boolean"] : ["number"]
		dropKeysRight:				isNumberComparer ? ["number", "ordered"] : isConditionalOp ? ["string", "boolean"]  : acceptsEverything ? ["boolean", "string", "number"] : acceptsBoolean ? ["boolean"] : ["number"]
		dropKeysMirrorEachother:	acceptsEverything

		x:							parent.dragX
		y:							parent.dragY
		isNested:					parent.nested

		acceptsDrops:				parent.acceptsDrops
	}
}
