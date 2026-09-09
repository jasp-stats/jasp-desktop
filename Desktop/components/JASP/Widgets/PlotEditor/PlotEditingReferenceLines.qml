import QtQuick
import QtQuick.Layouts
import JASP.Widgets
import JASP.Theme
import JASP.Controls
import JASP.PlotEditor
import JASP


ComponentsList
{
	id:					plotEditingReferenceLines
	visible:			count > 0
	isBound:			false
	headerLabels:		[qsTr("Type"), qsTr("Text"), qsTr("Hor."),qsTr("Vert."),qsTr("Color"),qsTr("Size"), qsTr("Style")]
	values:				plotEditorModel.references.count
	addBorder:			false

	Component.onCompleted: plotEditorModel.references.setItem(plotEditingReferenceLines)

	rowComponent: Row
	{
		id: rowId
		spacing: jaspTheme.contentMargin

		function getData(col)
		{
			return  plotEditorModel.references.data(plotEditorModel.references.index(rowIndex, col))
		}
		function setData(col, value)
		{
			plotEditorModel.references.setData(plotEditorModel.references.index(rowIndex, col), value)
		}


		DropDown
		{
			id: lineTypes
			name: "type"
			values: [
				{ value: 0, label: qsTr("Point") },
				{ value: 1, label: qsTr("Horizontal Line") },
				{ value: 2, label: qsTr("Vertical Line") }
			]
			startValue: rowId.getData(0)
			onValueChanged: rowId.setData(0, value)
		}

		TextField
		{
			name: "lineText"
			fieldWidth: 150 * jaspTheme.uiScale
			defaultValue: rowId.getData(1)
			onValueChanged: rowId.setData(1, value)
		}

		DoubleField
		{
			name: "lineHorizontal"
			enabled: lineTypes.value == 0 || lineTypes.value == 2
			negativeValues: true
			defaultValue: rowId.getData(2)
			onValueChanged: rowId.setData(2, value)
		}
		DoubleField
		{
			name: "lineVertical"
			enabled: lineTypes.value == 0 || lineTypes.value == 1
			negativeValues: true
			defaultValue: rowId.getData(3)
			onValueChanged: rowId.setData(3, value)
		}
		ColorPicker
		{
			name: "lineColor"
			buttonText: ""
			value: rowId.getData(4)
			onValueChanged: rowId.setData(4, value)
		}
		DoubleField
		{
			name: "lineSize"
			defaultValue: rowId.getData(5)
			onValueChanged: rowId.setData(5, value)

		}
		DropDown
		{
			name: "lineType"
			values: lineTypes.currentValue == 0 ? [
					{ value: "Circle",		label: qsTr("Circle") },
					{ value: "Square",		label: qsTr("Square") },
					{ value: "Triangle",	label: qsTr("Triangle") },
					{ value: "Diamond",		label: qsTr("Diamond") },
					{ value: "Cross",		label: qsTr("Cross") },
					{ value: "Star",		label: qsTr("Star") } ]
				: [
					{ value: "Solid",		label: qsTr("Solid") },
					{ value: "Dashed",		label: qsTr("Dashed") },
					{ value: "Dotted",		label: qsTr("Dotted") },
					{ value: "DotDash",		label: qsTr("Dot-Dash") },
					{ value: "LongDash",	label: qsTr("Long Dash") },
					{ value: "TwoDash",		label: qsTr("Two Dash") }
				]
			startValue: rowId.getData(6)
			onValueChanged: rowId.setData(6, value)
		}

	}


}
