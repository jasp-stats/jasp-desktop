import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0


Item {

	anchors.fill:		parent

	MenuHeader {
		id: menuHeader
		headertext:"Data Preferences"
		helpbutton: true
		helpfile: "preferences/prefsdata"
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

			CheckBox  //Synchronize automatically
			{
				label:				"Synchronize automatically on data file save"
				checked:			preferencesModel.dataAutoSynchronization
				onCheckedChanged:	preferencesModel.dataAutoSynchronization = checked
				//font:				Theme.font
			}

			Item //Use default spreadsheet editor
			{
				height:	useDefaultEditor.height + (editCustomEditor.visible ? editCustomEditor.height : 0)
				width:	parent.width - Theme.generalAnchorMargin

				CheckBox
				{
					id:					useDefaultEditor
					label:				"Use default spreadsheet editor"
					checked:			preferencesModel.useDefaultEditor
					onCheckedChanged:	preferencesModel.useDefaultEditor = checked
					//font:				Theme.font
				}

				Item
				{
					id:					editCustomEditor
					visible:			!preferencesModel.useDefaultEditor
					width:				parent.width
					height:				browseEditorButton.height
					anchors.top:		useDefaultEditor.bottom


					RectangularButton
					{
						id:					browseEditorButton
						text:				"Select custom editor"
						onClicked:			preferencesModel.browseSpreadsheetEditor()
						anchors.left:		parent.left
						anchors.leftMargin: Theme.subOptionOffset
					}

					Rectangle
					{
						anchors
						{
							left:			browseEditorButton.right
							right:			parent.right
							top:			parent.top
							bottom:			parent.bottom
						}

						height:				browseEditorButton.height
						color:				Theme.white
						border.color:		Theme.buttonBorderColor
						border.width:		1

						TextInput
						{
							id:					customEditorText
							text:				preferencesModel.customEditor
							clip:				true
							font:				Theme.font
							onTextChanged:		preferencesModel.customEditor = text
							color:				Theme.textEnabled

							anchors
							{
								left:			parent.left
								right:			parent.right
								verticalCenter:	parent.verticalCenter
								margins:		Theme.generalAnchorMargin
							}

							Connections
							{
								target:					preferencesModel
								onCustomEditorChanged:	customEditorText = preferencesModel.customEditor
							}

						}
					}
				}
			}


			Item  //Scale threshold
			{
				height:		customThreshold.height
				width:		customThreshold.width + thresholdScale.width

				CheckBox
				{
					id:					customThreshold
					label:				qsTr("Custom threshold between Scale or Nominal")
					checked:			preferencesModel.customThresholdScale
					onCheckedChanged:	preferencesModel.customThresholdScale = checked
					//font:				Theme.font
					toolTip:			qsTr("This will determine if, when importing new data, a column will be interpreted as a Scale column (When there are more unique integers then specified) or Nominal.")
				}

				SpinBox
				{
					id:					thresholdScale
					value:				preferencesModel.thresholdScale
					onValueChanged:		preferencesModel.thresholdScale = value
					anchors.left:		customThreshold.right
					anchors.leftMargin: Theme.generalAnchorMargin
					anchors.verticalCenter: parent.verticalCenter
					height:				Theme.spinBoxHeight//parent.height
					visible:			preferencesModel.customThresholdScale
					font:				Theme.font
					editable:			true
				}
			}


			PrefsMissingValues {} //Missing Value List
		}
	}
}
