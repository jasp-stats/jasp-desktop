import QtQuick 2.0

DragGeneric {
	property real value: 0

	dragKeys: ["number"]
	shownChild: showMe

	Number
	{
		id: showMe
		value: parent.value
		x: parent.dragX
		y: parent.dragY

	}
}
