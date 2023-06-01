import QtQuick 2.9


Item
{

					id:							opRoot
					objectName:					"Operator"
					height:						Math.max(filterConstructor.blockDim, leftDrop.height, rightDrop.height)
					width:						opX + opWidth + rightDrop.width + (haakjesRechts.visible ? haakjesRechts.width : 0) //(haakjesLinks.visible ? haakjesLinks.width : 0) + leftDrop.width + opWidth + rightDrop.width + (haakjesRechts.visible ? haakjesRechts.width : 0)

	property string	__debugName:				"Operator "+operator

	property int	initialWidth:				filterConstructor.blockDim * acceptsDrops ? 4 : 2
	property string operator:					"+"
	property string operatorImageSource:		""
	property bool	acceptsDrops:				true
	property bool	isNested:					false
	property var	leftDropSpot:				leftDrop
	property var	dropKeys:					["number"]
	property bool	dropKeysMirrorEachother:	false
	property var	dropKeysLeft:				dropKeys
	property var	dropKeysRight:				dropKeys

	property alias	leftDrop:					leftDrop
	property alias	rightDrop:					rightDrop

	property real	opWidth:					opImg.visible ? opImg.width + 2 : opText.width
	property real	opX:						opImg.visible ? opImg.x : opText.x

	function shouldDrag(mouseX, mouseY)
	{
		if(!acceptsDrops)
			return true

		return mouseX <= haakjesLinks.width || mouseX > haakjesRechts.x || ( mouseX > opX && mouseX < opX + opWidth);
	}

	function returnR()
	{
		var compounded = "("
		compounded += leftDrop.containsItem !== null ? leftDrop.containsItem.returnR() : "null"
		compounded += " " + operator + " "
		compounded += rightDrop.containsItem !== null ? rightDrop.containsItem.returnR() : "null"
		compounded += ")"

		return compounded
	}

	function returnEmptyRightMostDropSpot()
	{
		if(rightDrop.containsItem !== null)
			return rightDrop.containsItem.returnEmptyRightMostDropSpot()
		return rightDrop
	}

	function returnFilledRightMostDropSpot()
	{
		if(rightDrop.containsItem !== null)
			return rightDrop
		return null
	}

	function checkCompletenessFormulas()
	{
		var leftIsOk = leftDrop.checkCompletenessFormulas()
		var rightIsOk = rightDrop.checkCompletenessFormulas()
		return leftIsOk && rightIsOk
	}

	function convertToJSON()
	{
		var jsonObj = {
			"nodeType":			"Operator",
			"operator":			operator,
			"leftArgument":		(leftDrop.containsItem === null ? null : leftDrop.containsItem.convertToJSON()),
			"rightArgument":	(rightDrop.containsItem === null ? null : rightDrop.containsItem.convertToJSON())
		}
		return jsonObj
	}

	Text
	{
		id:						haakjesLinks
		anchors.top:			parent.top
		anchors.bottom:			parent.bottom
		//width: contentWidth

		verticalAlignment:		Text.AlignVCenter
		horizontalAlignment:	Text.AlignHCenter

		text:					opRoot.isNested ? "(" : ""
		font.pixelSize:			filterConstructor.fontPixelSize
		color:					jaspTheme.textEnabled

		visible:				opRoot.isNested
	}

	DropSpot {
		dropKeys: !(opRoot.dropKeysMirrorEachother && rightDrop.containsItem !== null) ? opRoot.dropKeysLeft : rightDrop.containsItem.dragKeys

		id: leftDrop
		x: haakjesLinks.width
		anchors.verticalCenter: parent.verticalCenter


		//width: acceptsDrops ? implicitWidth : 0
		//height: implicitHeight
		//implicitWidth: opRoot.initialWidth / 4
		//implicitHeight: filterConstructor.blockDim

		acceptsDrops: parent.acceptsDrops
		droppedShouldBeNested: true
	}

	Image
	{
		id: opImg
		x: leftDrop.x + leftDrop.width + 2

		visible: operatorImageSource !== ""

		source: operatorImageSource
		sourceSize.width: filterConstructor.blockDim * 2
		sourceSize.height: filterConstructor.blockDim * 2

		height: filterConstructor.blockDim
		width: height
		anchors.verticalCenter: parent.verticalCenter
	}

	Text
	{
		id:						opText
		anchors.top:			parent.top
		anchors.bottom:			parent.bottom
		//width:				max(opRoot.initialWidth / 4
		leftPadding:			2
		rightPadding:			2
		x:						leftDrop.x + leftDrop.width

		verticalAlignment:		Text.AlignVCenter
		horizontalAlignment:	Text.AlignHCenter

		text:					opRoot.operator
		font.pixelSize:			filterConstructor.fontPixelSize
		color:					jaspTheme.textEnabled
		visible:				!opImg.visible

		font.bold:				true
	}



	DropSpot {
		dropKeys: !(opRoot.dropKeysMirrorEachother && leftDrop.containsItem !== null) ? opRoot.dropKeysRight : leftDrop.containsItem.dragKeys

		anchors.verticalCenter: parent.verticalCenter

		id: rightDrop
		//height: implicitHeight
		//width: acceptsDrops ? implicitWidth : 0
		//implicitWidth: opRoot.initialWidth / 4
		//implicitHeight: filterConstructor.blockDim
		x: opX + opWidth

		acceptsDrops: parent.acceptsDrops
		droppedShouldBeNested: true
	}

	Text
	{
		id:						haakjesRechts
		anchors.top:			parent.top
		anchors.bottom:			parent.bottom
		//width:				opRoot.isNested ? opRoot.initialWidth / 8 : 0
		x:						rightDrop.x + rightDrop.width

		verticalAlignment:		Text.AlignVCenter
		horizontalAlignment:	Text.AlignHCenter

		//text: ")"
		text:					opRoot.isNested ? ")" : ""
		font.pixelSize:			filterConstructor.fontPixelSize
		color:					jaspTheme.textEnabled
		visible:				opRoot.isNested
	}
}
