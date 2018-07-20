import QtQuick 2.0


DragGeneric {
	shownChild                               : showMe
	property string       __debugName        : "OperatorDrag"

	readonly property var everythingOperators: ["==", "<", ">"]
	property string       operator           : "+"
	property          var opImages           : { '==': 'qrc:/icons/equal.png', '<': 'qrc:/icons/lessThan.png', '>': 'qrc:/icons/greaterThan.png'}
	property bool         acceptsDrops       : true

	leftDropSpot                             : showMe.leftDropSpot
	property alias leftDrop                  : showMe.leftDrop
	property alias rightDrop                 : showMe.rightDrop

	Operator
	{
		id                     : showMe
		operator               : parent.operator
		operatorImageSource    : parent.opImages[operator] !== null && parent.opImages[operator] !== undefined ? parent.opImages[operator] : ""

		dropKeysLeft           : ["boolean", "string", "number"]
		dropKeysRight          : ["boolean", "string", "number"]
		dropKeysMirrorEachother: true

		x                      : parent.dragX
		y                      : parent.dragY
		isNested               : parent.nested

		acceptsDrops           : parent.acceptsDrops
	}
}
