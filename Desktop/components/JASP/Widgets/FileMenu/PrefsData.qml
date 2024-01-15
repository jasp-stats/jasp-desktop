import QtQuick
import QtQuick.Controls as QTC
import JASP.Widgets
import JASP.Controls


QTC.ScrollView
{
	id:                     scrollPrefs
	focus:                  true
	onActiveFocusChanged:	if(activeFocus) useDefaultEditor.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	Column
	{

		width:		scrollPrefs.width
		spacing:	jaspTheme.rowSpacing

		MenuHeader
		{
			id:			menuHeader
			headertext:	qsTr("Data Preferences")
			helpfile:	"preferences/PrefsData"
			anchorMe:	false
			width:		scrollPrefs.width - (2 * jaspTheme.generalMenuMargin)
			x:			jaspTheme.generalMenuMargin
		}

		PrefsGroupRect
		{
			id:				spreadSheetEditor
			title:			qsTr("External spreadsheet editor (for data synchronization)")

			Item
			{
				height:		editCustomEditor.y + editCustomEditor.height
				width:		parent.width

				CheckBox
				{
					id:                     useDefaultEditor
					label:                  qsTr("Use default spreadsheet editor")
					checked:                LINUX || preferencesModel.useDefaultEditor
					onCheckedChanged:       preferencesModel.useDefaultEditor = checked
					enabled:                !LINUX

					KeyNavigation.tab:      editCustomEditor
				}

				Label
				{
					id:					linuxInfo
					text:				qsTr("<i>On Linux the default spreadsheet editor is always used.</i>")
					visible:			LINUX
					textFormat:			Text.StyledText

					anchors
					{
						top:			useDefaultEditor.bottom
						left:			useDefaultEditor.left
						leftMargin:		jaspTheme.subOptionOffset
					}
				}

				Item
				{
					id:					editCustomEditor
					enabled:			!LINUX && !preferencesModel.useDefaultEditor
					width:				parent.width
					height:				browseEditorButton.height
					anchors.top:		useDefaultEditor.bottom

					RoundedButton
					{
						id:					browseEditorButton
						text:				qsTr("Select custom editor")
						onClicked:			preferencesModel.browseSpreadsheetEditor()
						anchors.left:		parent.left
						anchors.leftMargin: jaspTheme.subOptionOffset

						KeyNavigation.tab:      customEditorText
						activeFocusOnTab:		true
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
						color:				jaspTheme.white
						border.color:		jaspTheme.buttonBorderColor
						border.width:		1

						TextInput
						{
							id:					customEditorText
							text:				preferencesModel.customEditor
							clip:				true
							font:				jaspTheme.font
							onTextChanged:		preferencesModel.customEditor = text
							color:				jaspTheme.textEnabled

							KeyNavigation.tab:      thresholdScale

							anchors
							{
								left:			parent.left
								right:			parent.right
								verticalCenter:	parent.verticalCenter
								margins:		jaspTheme.generalAnchorMargin
							}

							Connections
							{
								target:					preferencesModel
								function onCustomEditorChanged(customEditor) { customEditorText.text = customEditor; }
							}
						}
					}
				}
			}
		}

		PrefsGroupRect
		{
			id:				thresholdScaleSetting
			title:			qsTr("Import threshold between Categorical or Scale")

			SpinBox
			{
				id:					thresholdScale
				value:				preferencesModel.thresholdScale
				onValueChanged:		preferencesModel.thresholdScale = value

				KeyNavigation.tab:	resetDataWithThresholdButton

				Button
				{
					id:				resetDataWithThresholdButton
					label:			qsTr("Reset variable types")
					visible:		mainWindow.dataAvailable
					onClicked:		mainWindow.resetVariableTypes()
					anchors
					{
						top: thresholdScale.top
						left: thresholdScale.right
						leftMargin: jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	missingValueDataLabelInput
				}
			}
		}

		PrefsGroupRect
		{
			id:				missingValuesSettings
			title:			qsTr("Missing values setting")

			Item
			{
				id:				missingValueDataLabelItem
				height:			missingValueDataLabelInput.height

				Label
				{
					id:					missingValueDataLabelLabel
					text:				qsTr("Show missing values as: ")

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
					}
				}

				PrefsTextInput
				{
					id:					missingValueDataLabelInput

					text:				preferencesModel.dataLabelNA
					onEditingFinished:	preferencesModel.dataLabelNA = text
					nextEl:				missingValuesList

					anchors
					{
						left:		missingValueDataLabelLabel.right
						right:		parent.right
					}
				}
			}

			Item
			{
				width:	parent.width
				height: missingValuesList.height

				PrefsMissingValues
				{
					id:							missingValuesList
					model:						preferencesModel
					showResetWorkspaceButton:	true
					resetButtonLabel:			qsTr("Reset with standard values")
					resetButtonTooltip:			qsTr("Reset missing values with the standard JASP missing values")
					KeyNavigation.tab:			WINDOWS ? noBomNative : useDefaultEditor
				}
			}
		}

		PrefsGroupRect
		{
			visible:	WINDOWS
			enabled:	WINDOWS
			title:		qsTr("Windows workaround")

			CheckBox
			{
				id:					noBomNative
				label:				qsTr("Assume CSV is the selected codepage, when no BOM is specified.")
				checked:			preferencesModel.windowsNoBomNative
				onCheckedChanged:	preferencesModel.windowsNoBomNative = checked
				toolTip:			qsTr("See documentation for more information ")

				KeyNavigation.tab:		codePageSelection
			}

			/*ErrorMessage
			{
				text: WINDOWS && windowsCodePagesHelper.error ? qsTr("Some problem occured loading the available codepages...") : ""
			}*/


			DropDown
			{
				id:			 			codePageSelection
				enabled:				preferencesModel.windowsNoBomNative && WINDOWS //&& !windowsCodePagesHelper.error
				toolTip:				qsTr("See documentation for more information ")
				values:			 		WINDOWS ? windowsCodePagesHelper.codePageIDs : []
				addEmptyValue:			true
				showEmptyValueAsNormal:	true
				addLineAfterEmptyValue:	true
				placeholderText:		qsTr("Choose codepage here")
				startValue:				WINDOWS ? windowsCodePagesHelper.codePageID : ""
				onValueChanged: 		if(WINDOWS) windowsCodePagesHelper.codePageID = value

				KeyNavigation.tab:		useDefaultEditor
			}
		}
	}
}
