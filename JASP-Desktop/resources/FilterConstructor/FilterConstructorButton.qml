import QtQuick 2.0

Text
{
	text: "Button"
	property string __debugName: "FilterConstructorButton"

	property var onClickedFunction: function () {}

	MouseArea
	{
		id: mouseAreaButton
		anchors.fill: parent
		onClicked: { focus = true; onClickedFunction()}

		hoverEnabled: true

		onPositionChanged:
		{
			cursorShape = containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
		}
	}

	horizontalAlignment: Text.AlignHCenter
	verticalAlignment: Text.AlignVCenter


	property real contentPadding: 15
	width: contentWidth + contentPadding
	height: contentHeight + 0.5 * contentPadding

	Rectangle
	{
		color: mouseAreaButton.containsMouse ? " white" : "transparent"
		border.color: "white"
		border.width: 1
		anchors.fill: parent
		z: -1
	}

}
