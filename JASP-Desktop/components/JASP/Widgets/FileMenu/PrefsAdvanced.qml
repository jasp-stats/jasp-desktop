import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) uiScaleSpinBox.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	Column
	{
		width:			scrollPrefs.width
		spacing:		Theme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("Advanced Preferences") + languageModel.emptyString
			helpfile:		"preferences/prefsadvanced"
			anchorMe:		false
			width:			scrollPrefs.width - (2 * Theme.generalMenuMargin)
			x:				Theme.generalMenuMargin

		}

		PrefsGroupRect
		{
			title:						qsTr("User interface options")

			SpinBox
			{
				id:						uiScaleSpinBox
				value:					Math.round(preferencesModel.uiScale * 100)
				onEditingFinished:		preferencesModel.uiScale = value / 100
				from:					20
				to:						300
				stepSize:				10
				decimals:				0
				text:					qsTr("Zoom (%): ")
				toolTip:				qsTr("Increase or decrease the size of the interface elements (text, buttons, etc).")

				KeyNavigation.tab:		uiMaxFlickVelocity
				KeyNavigation.down:		uiMaxFlickVelocity

				widthLabel:				Math.max(uiScaleSpinBox.implicitWidthLabel, uiMaxFlickVelocity.implicitWidthLabel)
			}

			SpinBox
			{
				id:						uiMaxFlickVelocity
				value:					preferencesModel.maxFlickVelocity
				onValueChanged:			if(value !== "") preferencesModel.maxFlickVelocity = value
				from:					100
				to:						3000
				stepSize:				100
				decimals:				0
				text:					qsTr("Scroll speed (pix/s): ")
				toolTip:				qsTr("Set the speed with which you can scroll in the options, dataviewer and other places.")
				widthLabel:				uiScaleSpinBox.widthLabel

				KeyNavigation.tab:		rememberModulesSelected
				KeyNavigation.down:		rememberModulesSelected
			}

			CheckBox
			{
				id:					rememberModulesSelected
				label:				qsTr("Remember Enabled Modules")
				checked:			preferencesModel.modulesRemember
				onCheckedChanged:	preferencesModel.modulesRemember = checked
				toolTip:			qsTr("Continue where you left of the next time JASP starts.\nEnabling this option makes JASP remember which Modules you've enabled.")
				KeyNavigation.tab:	safeGraphicsMode
				KeyNavigation.down:	safeGraphicsMode
			}

			CheckBox
			{
				id:					safeGraphicsMode
				label:				qsTr("Safe Graphics Mode")
				checked:			preferencesModel.safeGraphics
				onCheckedChanged:	preferencesModel.safeGraphics = checked
				toolTip:			qsTr("Switches to a \"safer\" mode for graphics aka software rendering.\nIt will make your interface slower but if you have some problems (weird glitches, cannot see results or anything even) might fix them.\nAnalyses will still be just as fast though.")
				KeyNavigation.tab:	developerMode
				KeyNavigation.down:	developerMode
			}
		}

		PrefsGroupRect
		{
			title:				qsTr("Modules options")

			CheckBox
			{
				id:					developerMode
				label:				qsTr("Developer mode (Beta version)")
				checked:			preferencesModel.developerMode
				onCheckedChanged:	preferencesModel.developerMode = checked
				toolTip:			qsTr("To use JASP Modules enable this option.")
				KeyNavigation.tab:	browseDeveloperFolderButton
				KeyNavigation.down:	browseDeveloperFolderButton
			}

			Item
			{
				id:					editDeveloperFolder
				visible:			preferencesModel.developerMode
				width:				parent.width
				height:				browseDeveloperFolderButton.height + overwriteDescriptionEtc.height

				RectangularButton
				{
					id:					browseDeveloperFolderButton
					text:				qsTr("Select developer folder")
					onClicked:			preferencesModel.browseDeveloperFolder()
					anchors.left:		parent.left
					anchors.leftMargin: Theme.subOptionOffset
					toolTip:			qsTr("Browse to your JASP Module folder.")
					KeyNavigation.tab:	developerFolderText
					KeyNavigation.down:	developerFolderText
				}

				Rectangle
				{
					id:					developerFolderTextRect
					anchors
					{
						left:			browseDeveloperFolderButton.right
						right:			parent.right
						top:			parent.top
					}

					height:				browseDeveloperFolderButton.height
					color:				Theme.white
					border.color:		Theme.buttonBorderColor
					border.width:		1

					TextInput
					{
						id:					developerFolderText
						text:				preferencesModel.developerFolder
						clip:				true
						font:				Theme.font
						onTextChanged:		preferencesModel.developerFolder = text
						color:				Theme.textEnabled
						KeyNavigation.tab:	overwriteDescriptionEtc
						KeyNavigation.down:	overwriteDescriptionEtc
						selectByMouse:		true

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
							onCustomEditorChanged:	developerFolderText = preferencesModel.developerFolder
						}

					}
				}

				CheckBox
				{
					id:					overwriteDescriptionEtc
					label:				qsTr("Regenerate package metadata every time (DESCRIPTION & NAMESPACE)")
					checked:			preferencesModel.devModRegenDESC
					onCheckedChanged:	preferencesModel.devModRegenDESC = checked
					height:				implicitHeight * preferencesModel.uiScale
					toolTip:			qsTr("Disable this option if you are transforming your R-package to a JASP Module or simply want to keep manual changes to DESCRIPTION and NAMESPACE.")
					KeyNavigation.tab:	cranRepoUrl
					KeyNavigation.down:	cranRepoUrl

					anchors
					{
						left:			parent.left
						leftMargin:		Theme.subOptionOffset
						top:			developerFolderTextRect.bottom
					}
				}
			}

			Item
			{
				id:		cranRepoUrlItem
				width:	parent.width
				height:	cranRepoUrlRect.height

				Label
				{
					id:		cranRepoUrlLabel
					text:	qsTr("Change the CRAN repository: ")

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						margins:		Theme.generalAnchorMargin
					}
				}

				Rectangle
				{
					id:					cranRepoUrlRect

					height:				browseDeveloperFolderButton.height

					color:				Theme.white
					border.color:		Theme.buttonBorderColor
					border.width:		1

					anchors
					{
						left:		cranRepoUrlLabel.right
						right:		parent.right
					}

					TextInput
					{
						id:					cranRepoUrl
						text:				preferencesModel.cranRepoURL
						clip:				true
						font:				Theme.font
						onTextChanged:		preferencesModel.cranRepoURL = text
						color:				Theme.textEnabled
						KeyNavigation.tab:	logToFile
						KeyNavigation.down:	logToFile
						selectByMouse:		true

						anchors
						{
							left:			parent.left
							right:			parent.right
							verticalCenter:	parent.verticalCenter
							margins:		Theme.generalAnchorMargin
						}
					}
				}
			}
		}

		PrefsGroupRect
		{
			id:		loggingGroup
			title:	qsTr("Logging options")

			CheckBox
			{
				id:					logToFile
				label:				qsTr("Log to file")
				checked:			preferencesModel.logToFile
				onCheckedChanged:	preferencesModel.logToFile = checked
				toolTip:			qsTr("To store debug-logs of JASP in a file, check this box.")
				KeyNavigation.tab:	maxLogFilesSpinBox
				KeyNavigation.down:	maxLogFilesSpinBox
			}

			Item
			{
				id:					loggingSubGroup
				x:					Theme.subOptionOffset
				height:				maxLogFilesSpinBox.height
				width:				showLogs.x + showLogs.width
				enabled:			preferencesModel.logToFile


				SpinBox
				{
					id:					maxLogFilesSpinBox
					value:				preferencesModel.logFilesMax
					onValueChanged:		if(value !== "") preferencesModel.logFilesMax = value
					from:				5 //Less than 5 makes no sense as on release you get 1 for JASP-Desktop and 4 from the Engines
					to:					100
					defaultValue:		10
					stepSize:			1
					KeyNavigation.tab:	showLogs
					KeyNavigation.down:	showLogs
					text:				qsTr("Max logfiles to keep: ")

					anchors
					{
						leftMargin:	Theme.generalAnchorMargin
						left:		parent.left
						top:		showLogs.top
						bottom:		showLogs.bottom
					}
				}

				RectangularButton
				{
					id:			showLogs
					text:		qsTr("Show logs")
					onClicked:	mainWindow.showLogFolder();
					anchors
					{
						margins:	Theme.generalAnchorMargin
						left:		maxLogFilesSpinBox.right
					}
					KeyNavigation.tab:	uiScaleSpinBox
					KeyNavigation.down:	uiScaleSpinBox
				}
			}
		}

		PrefsGroupRect
		{
			id:		languageGroup
			title:	qsTr("Preferred Language")

			ComboBox
			{
				id:			languages
				fieldWidth: 100

				label:		qsTr("Choose Language  ") + languageModel.emptyString

				useModelDefinedIcon: true
				isDirectModel:		true

				currentIndex:		languageModel.currentIndex

				model: languageModel

				onActivated:{
					languageModel.currentIndex =index
					languageModel.updateLanguage(index, scrollPrefs);
				}
			}

		}

		Item
		{
			id:		extraSpaceForScrolling
			width:	1
			height:	1
		}
	}
}
