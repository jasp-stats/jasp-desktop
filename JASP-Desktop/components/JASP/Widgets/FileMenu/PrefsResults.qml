import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0
import JASP.Theme		1.0

Column
{
	anchors.fill:		parent
	anchors.margins:	Theme.generalAnchorMargin
	spacing:			Theme.rowSpacing

	CheckBox
	{
		text:				"Display exact p-values"
		checked:			preferencesModel.exactPValues
		onCheckedChanged:	preferencesModel.exactPValues = checked
		font:				Theme.font
	}

	Item
	{
		height:		fixDecs.height
		width:		fixDecs.width + numDecs.width

		CheckBox
		{
			id:					fixDecs
			text:				"Fix the number of decimals"
			checked:			preferencesModel.fixedDecimals
			onCheckedChanged:	preferencesModel.fixedDecimals = checked
			font:				Theme.font
		}

		SpinBox
		{
			id:					numDecs
			value:				preferencesModel.numDecimals
			onValueChanged:		preferencesModel.numDecimals = value
			anchors.left:		fixDecs.right
			anchors.leftMargin: Theme.generalAnchorMargin
			anchors.top:		parent.top
			height:				parent.height
			visible:			preferencesModel.fixedDecimals
			font:				Theme.font
		}
	}
}
