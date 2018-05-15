import QtQuick 2.0

DragGeneric {
	property string columnName: "?"
	property string columnIcon: ""

	shownChild: showMe
	dragKeys: showMe.dragKeys
	property real colScaler: 1

	JASPColumn
	{
		id: showMe
		columnName: parent.columnName
		columnIcon: parent.columnIcon

		x: parent.dragX
		y: parent.dragY

		colScaler: parent.colScaler

	}
}
