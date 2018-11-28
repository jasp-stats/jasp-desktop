import QtQuick 2.0

DragGeneric {
	property string columnName: "?"
	property string columnIcon: ""

	shownChild: showMe
	dragKeys: showMe.dragKeys

	JASPColumn
	{
		id: showMe
		columnName: parent.columnName
		columnIcon: parent.columnIcon

		x: parent.dragX
		y: parent.dragY
	}
}
