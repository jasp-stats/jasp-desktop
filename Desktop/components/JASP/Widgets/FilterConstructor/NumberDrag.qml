import QtQuick 2.0

DragGeneric {
	property real value: 0

	dragKeys: ["number"]
	shownChild: showMe
	property string __debugName: "NumberDrag"

	Number
	{
		id: showMe
		value: parent.value
		x: parent.dragX
		y: parent.dragY

	}
}
