import QtQuick 2.0

DragGeneric {
	property string text: "?"

	dragKeys: ["string"]
	shownChild: showMe

	String
	{
		id: showMe
		text: parent.text
		x: parent.dragX
		y: parent.dragY

	}
}
