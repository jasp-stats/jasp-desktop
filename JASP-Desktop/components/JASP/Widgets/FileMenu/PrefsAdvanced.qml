import QtQuick			2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3
import JASP.Widgets		1.0
import JASP.Theme		1.0
import JASP.Controls	1.0
import JASP				1.0

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) useDefaultPPICheckbox.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	Column
	{
		width:			scrollPrefs.width
		//anchors.left:	parent.left
		//anchors.right:	parent.right
		spacing:		Theme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		"Advanced Preferences"
			helpfile:		"preferences/prefsadvanced"
			anchorMe:		false
			width:			scrollPrefs.width
		}

		PrefsGroupRect
		{
			title:				qsTr("Plot options")

			CheckBox
			{
				id:					useDefaultPPICheckbox
				label:				"Use PPI of screen in plots: " + preferencesModel.defaultPPI
				checked:			preferencesModel.useDefaultPPI
				onCheckedChanged:	preferencesModel.useDefaultPPI = checked
				height:				implicitHeight * preferencesModel.uiScale
				toolTip:			qsTr("Use the Pixels per Inch of your screen to render your plots")
				focus:				true
				KeyNavigation.tab:	customPPISpinBox
				KeyNavigation.down:	customPPISpinBox
			}

			SpinBox
			{
				id:						customPPISpinBox
				value:					preferencesModel.customPPI
				onValueChanged:			preferencesModel.customPPI = value
				from:					16
				to:						2000
				stepSize:				16

				KeyNavigation.tab:		whiteBackgroundButton
				KeyNavigation.down:		whiteBackgroundButton
				text:					qsTr("Custom PPI: ")
				enabled:				!preferencesModel.useDefaultPPI

				x:						Theme.subOptionOffset
			}



			RadioButtonGroup
			{
				title:					qsTr("Image Background Color")

				RadioButton
				{
					id:					whiteBackgroundButton
					text:				qsTr("White")
					checked:			preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = checked
					toolTip:			qsTr("This makes the background of all plots white, quite useful if you want to use it in LaTeX or submit it to a journal.")
					KeyNavigation.tab:	transparentBackgroundButton
					KeyNavigation.down:	transparentBackgroundButton
				}

				RadioButton
				{
					id:					transparentBackgroundButton
					text:				qsTr("Transparent")
					checked:			!preferencesModel.whiteBackground
					onCheckedChanged:	preferencesModel.whiteBackground = !checked
					toolTip:			qsTr("This makes the background of all plots transparent, quite useful if you want to use it seamlessly on any background that isn't white.")
					KeyNavigation.tab:	uiScaleSpinBox
					KeyNavigation.down:	uiScaleSpinBox
				}
			}
		}

		PrefsGroupRect
		{
			title:						qsTr("User interface options")

			SpinBox
			{
				id:						uiScaleSpinBox
				value:					preferencesModel.uiScale
				onValueChanged:			if(value !== "") preferencesModel.uiScale = value
				from:					0.01
				to:						3
				stepSize:				0.1
				decimals:				2
				text:					qsTr("Scaling: ")
				toolTip:				qsTr("How big do you want the interface elements (text, buttons etc) to be?")

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
				text:					qsTr("Max Flick Velocity (pix/s): ")
				toolTip:				qsTr("Flick velocity is how fast the scrolling can get in the options, dataviewer and other places.")
				widthLabel:				uiScaleSpinBox.widthLabel

				KeyNavigation.tab:		developerMode
				KeyNavigation.down:		developerMode
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
				toolTip:			qsTr("To use JASP Modules enable this option")
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
					toolTip:			qsTr("Browse to your JASP Module folder")
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
					//font:				Theme.font
					height:				implicitHeight * preferencesModel.uiScale
					toolTip:			qsTr("Disable this option if you are transforming your R-package to a JASP Module or simply want to keep manual changes to DESCRIPTION and NAMESPACE.")
					KeyNavigation.tab:	logToFile
					KeyNavigation.down:	logToFile

					anchors
					{
						left:			parent.left
						leftMargin:		Theme.subOptionOffset
						top:			developerFolderTextRect.bottom
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
					KeyNavigation.tab:	useDefaultPPICheckbox
					KeyNavigation.down:	useDefaultPPICheckbox
				}
			}
		}
	}
}
