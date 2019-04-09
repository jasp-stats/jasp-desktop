import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0

Item {

	anchors.fill:		parent

	MenuHeader {
		id: menuHeader
		headertext:"Results Preferences"
	}

	ScrollView
	{
		id:				scrollPrefs
		anchors.top:	menuHeader.bottom
		anchors.left:	menuHeader.left
		anchors.right:	menuHeader.right
		anchors.bottom: menuHeader.bottom
		anchors.topMargin: 2 * Theme.generalMenuMargin

		Column
		{
			width:			scrollPrefs.width
			spacing:		Theme.rowSpacing

			CheckBox
			{
				label:				"Display exact p-values"
				checked:			preferencesModel.exactPValues
				onCheckedChanged:	preferencesModel.exactPValues = checked
				//font:				Theme.font
			}

			Item
			{
				height:		fixDecs.height
				width:		fixDecs.width + numDecs.width

				CheckBox
				{
					id:					fixDecs
					label:				qsTr("Fix the number of decimals")
					checked:			preferencesModel.fixedDecimals
					onCheckedChanged:	preferencesModel.fixedDecimals = checked
					//font:				Theme.font
				}

				SpinBox
				{
					id:					numDecs
					value:				preferencesModel.numDecimals
					onValueChanged:		preferencesModel.numDecimals = value
					anchors.left:		fixDecs.right
					anchors.leftMargin: Theme.generalAnchorMargin
					anchors.verticalCenter: parent.verticalCenter
					height:				Theme.spinBoxHeight//parent.height
					visible:			preferencesModel.fixedDecimals
					font:				Theme.font
				}
			}
		}
	}
}
