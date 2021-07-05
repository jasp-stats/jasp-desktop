import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0

import JASP.Controls	1.0


Item
{
	focus:					true
	onActiveFocusChanged:	if(activeFocus) synchronizeDataSave.forceActiveFocus();

	MenuHeader
	{
		id:			menuHeader
		headertext:	qsTr("Data Preferences")
		helpfile:	"preferences/prefsdata"
	}

	ScrollView
	{
		id:					scrollPrefs
		anchors.top:		menuHeader.bottom
		anchors.left:		menuHeader.left
		anchors.right:		menuHeader.right
		anchors.bottom:		menuHeader.bottom
		anchors.topMargin:	jaspTheme.generalMenuMargin
		focus:				true
		Keys.onLeftPressed: resourceMenu.forceActiveFocus();
		focusPolicy:		Qt.WheelFocus

		PrefsGroupRect
		{
			spacing:		jaspTheme.rowSpacing
			implicitWidth:	scrollPrefs.width - (jaspTheme.generalAnchorMargin * 2)

			CheckBox  //Synchronize automatically
			{
				id:					synchronizeDataSave
				label:				qsTr("Synchronize automatically on data file save")
				checked:			preferencesModel.dataAutoSynchronization
				onCheckedChanged:	preferencesModel.dataAutoSynchronization = checked
				KeyNavigation.down:	useDefaultEditor
				KeyNavigation.tab:	useDefaultEditor
			}

			Item //Use default spreadsheet editor
			{

				height:		useDefaultEditor.height + (editCustomEditor.visible ? editCustomEditor.height : linuxInfo.visible ? linuxInfo.height : 0)
				width:		parent.width - jaspTheme.generalAnchorMargin

				CheckBox
				{
					id:					useDefaultEditor
					label:				qsTr("Use default spreadsheet editor")
					checked:			LINUX || preferencesModel.useDefaultEditor
					onCheckedChanged:	preferencesModel.useDefaultEditor = checked
					KeyNavigation.down:	browseEditorButton
					KeyNavigation.tab:	browseEditorButton
					enabled:			!LINUX
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
					visible:			!LINUX && !preferencesModel.useDefaultEditor
					width:				parent.width
					height:				browseEditorButton.height
					anchors.top:		useDefaultEditor.bottom

					RectangularButton
					{
						id:					browseEditorButton
						text:				qsTr("Select custom editor")
						onClicked:			preferencesModel.browseSpreadsheetEditor()
						anchors.left:		parent.left
						anchors.leftMargin: jaspTheme.subOptionOffset
						KeyNavigation.down:	customEditorText
						KeyNavigation.tab:	customEditorText
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
							KeyNavigation.down:	customThreshold
							KeyNavigation.tab:	customThreshold

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


			Item  //Scale threshold
			{
				height:		customThreshold.height
				width:		customThreshold.width + thresholdScale.width

				CheckBox
				{
					id:					customThreshold
					label:				qsTr("Import threshold between Categorical or Scale")
					checked:			preferencesModel.customThresholdScale
					onCheckedChanged:	preferencesModel.customThresholdScale = checked
					ToolTip.delay:		500
					ToolTip.timeout:	6000 //Some longer to read carefully
					toolTip:			qsTr("Threshold number of unique integers before classifying a variable as 'scale'.\nYou need to reload your data to take effect! Check help for more info.")
					KeyNavigation.down:	thresholdScale
					KeyNavigation.tab:	thresholdScale

				}

				SpinBox
				{
					id:					thresholdScale
					value:				preferencesModel.thresholdScale
					onValueChanged:		preferencesModel.thresholdScale = value
					visible:			preferencesModel.customThresholdScale

					KeyNavigation.down:	missingFileList.firstComponent
					KeyNavigation.tab:	missingFileList.firstComponent
					anchors
					{
						left:			customThreshold.right
						leftMargin:		jaspTheme.generalAnchorMargin
						verticalCenter:	parent.verticalCenter
					}
				}
			}

			PrefsMissingValues { id: missingFileList; navigateAfter: noBomNative }
			
			PrefsGroupRect
			{
				visible:	WINDOWS
				title:		qsTr("Windows workaround")
				
				CheckBox
				{
					id:					noBomNative
					label:				qsTr("Assume CSV is in native encoding when no BOM has been specified")
					checked:			preferencesModel.windowsNoBomNative
					onCheckedChanged:	preferencesModel.windowsNoBomNative = checked
					toolTip:			qsTr("See documentation for more information ")
					KeyNavigation.down:	synchronizeDataSave
					KeyNavigation.tab:	synchronizeDataSave

				}
			}
		}
	}
}
