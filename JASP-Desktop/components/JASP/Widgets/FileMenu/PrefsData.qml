import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0

ScrollView
{
	id:					scrollPrefs
	anchors.fill:		parent
	anchors.margins:	Theme.generalAnchorMargin

	Column
	{
		width:			scrollPrefs.width
		spacing:		Theme.rowSpacing

		CheckBox
		{
			text:				"Synchronize automatically on data file save"
			checked:			preferencesModel.dataAutoSynchronization
			onCheckedChanged:	preferencesModel.dataAutoSynchronization = checked
			//font:				Theme.font
		}

		Item
		{
			height:	useDefaultEditor.height + (editCustomEditor.visible ? editCustomEditor.height : 0)
			width:	parent.width - Theme.generalAnchorMargin

			CheckBox
			{
				id:					useDefaultEditor
				text:				"Use default spreadsheet editor"
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

		PrefsMissingValues {}
	}
}
