import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0

FocusScope
{
	focus:					true
	onActiveFocusChanged:	if(activeFocus) displayExactPVals.forceActiveFocus();

	MenuHeader
	{
		id:			menuHeader
		headertext:	"Results Preferences"
		helpfile:	"preferences/prefsresults"
	}

	ScrollView
	{
		id:					scrollPrefs
		anchors.top:		menuHeader.bottom
		anchors.left:		menuHeader.left
		anchors.right:		menuHeader.right
		anchors.bottom:		menuHeader.bottom
		anchors.topMargin:	Theme.generalMenuMargin
		focus:				true
		Keys.onLeftPressed: resourceMenu.forceActiveFocus();
		focusPolicy:		Qt.WheelFocus

		PrefsGroupRect
		{
			spacing:		Theme.rowSpacing
			implicitWidth:	scrollPrefs.width - (Theme.generalAnchorMargin * 2)

			CheckBox
			{
				id:						displayExactPVals
				label:					"Display exact p-values"
				checked:				preferencesModel.exactPValues
				onCheckedChanged:		preferencesModel.exactPValues = checked
				KeyNavigation.tab:		fixDecs
				KeyNavigation.backtab:	numDecs
				KeyNavigation.down:		fixDecs
			}

			Item
			{
				height:						fixDecs.height
				width:						fixDecs.width + numDecs.width

				CheckBox
				{
					id:						fixDecs
					label:					qsTr("Fix the number of decimals")
					checked:				preferencesModel.fixedDecimals
					onCheckedChanged:		preferencesModel.fixedDecimals = checked
					KeyNavigation.tab:		numDecs
					KeyNavigation.backtab:	displayExactPVals
					KeyNavigation.down:		numDecs
				}

				SpinBox
				{
					id:						numDecs
					value:					preferencesModel.numDecimals
					onValueChanged:			preferencesModel.numDecimals = value
					visible:				preferencesModel.fixedDecimals

					KeyNavigation.tab:		displayExactPVals
					KeyNavigation.backtab:	fixDecs
					KeyNavigation.down:		displayExactPVals
					anchors
					{
						left:				fixDecs.right
						leftMargin:			Theme.generalAnchorMargin
						verticalCenter:		parent.verticalCenter
					}
				}
			}
		}
	}
}
