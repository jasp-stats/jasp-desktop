import QtQuick

DragGeneric {
	property bool value: false

	dragKeys: ["boolean"]
	shownChild: showMe
	property string __debugName: "BooleanDrag"

	
	Boolean
	{
		id: showMe
		value: parent.value
		x: parent.dragX
		y: parent.dragY

	}
}
