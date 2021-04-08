import QtQuick			2.12
import QtQuick.Controls 2.12
import JASP.Widgets		1.0
import JASP.Controls	1.0

ScrollView
{
	id:						scrollPrefs
	focus:					true
	onActiveFocusChanged:	if(activeFocus) rememberModulesSelected.forceActiveFocus();
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	Column
	{
		width:			scrollPrefs.width
		spacing:		jaspTheme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("Advanced Preferences")
			helpfile:		"preferences/prefsadvanced"
			anchorMe:		false
			width:			scrollPrefs.width - (2 * jaspTheme.generalMenuMargin)
			x:				jaspTheme.generalMenuMargin
		}


		PrefsGroupRect
		{
			title:				qsTr("Modules options")

			CheckBox
			{
				id:					rememberModulesSelected
				label:				qsTr("Remember enabled modules")
				checked:			preferencesModel.modulesRemember
				onCheckedChanged:	preferencesModel.modulesRemember = checked
				toolTip:			qsTr("Continue where you left of the next time JASP starts.\nEnabling this option makes JASP remember which Modules you've enabled.")
				KeyNavigation.tab:	developerMode
				KeyNavigation.down:	developerMode
			}

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
				height:				browseDeveloperFolderButton.height

				RectangularButton
				{
					id:					browseDeveloperFolderButton
					text:				qsTr("Select developer folder")
					onClicked:			preferencesModel.browseDeveloperFolder()
					anchors.left:		parent.left
					anchors.leftMargin: jaspTheme.subOptionOffset
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
					color:				jaspTheme.white
					border.color:		jaspTheme.buttonBorderColor
					border.width:		1

					TextInput
					{
						id:					developerFolderText
						text:				preferencesModel.developerFolder
						clip:				true
						font:				jaspTheme.font
						onTextChanged:		preferencesModel.developerFolder = text
						color:				jaspTheme.textEnabled
						KeyNavigation.tab:	cranRepoUrlItem
						KeyNavigation.down:	cranRepoUrlItem
						selectByMouse:		true
						selectedTextColor:	jaspTheme.white
						selectionColor:		jaspTheme.itemSelectedColor


						anchors
						{
							left:			parent.left
							right:			parent.right
							verticalCenter:	parent.verticalCenter
							margins:		jaspTheme.generalAnchorMargin
						}

						Connections
						{
							target:			preferencesModel
							function onCustomEditorChanged(customEditor) { developerFolderText = preferencesModel.developerFolder; }
						}

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
						margins:		jaspTheme.generalAnchorMargin
					}
				}

				Rectangle
				{
					id:					cranRepoUrlRect

					height:				browseDeveloperFolderButton.height

					color:				jaspTheme.white
					border.color:		jaspTheme.buttonBorderColor
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
						font:				jaspTheme.font
						onTextChanged:		preferencesModel.cranRepoURL = text
						color:				jaspTheme.textEnabled
						KeyNavigation.tab:	generateMarkdown
						KeyNavigation.down:	generateMarkdown
						selectByMouse:		true
						selectedTextColor:	jaspTheme.white
						selectionColor:		jaspTheme.itemSelectedColor


						anchors
						{
							left:			parent.left
							right:			parent.right
							verticalCenter:	parent.verticalCenter
							margins:		jaspTheme.generalAnchorMargin
						}
					}
				}
			}

			CheckBox
			{
				id:					generateMarkdown
				label:				qsTr("Generate markdown files for help")
				checked:			preferencesModel.generateMarkdown
				onCheckedChanged:	preferencesModel.generateMarkdown = checked
				toolTip:			qsTr("Enabling this will generate markdown helpfile from the info at qml options.")
				KeyNavigation.tab:	checkForLC_CTYPE_C
				KeyNavigation.down:	checkForLC_CTYPE_C

			}
		}
		
		PrefsGroupRect
		{
			id:			windowsSpecific
			visible:	WINDOWS
			enabled:	WINDOWS
			title:		qsTr("Windows workarounds")
			
			RadioButtonGroup
			{
				title:	qsTr("Handling LC_CTYPE");
			
				RadioButton
				{
					id:					checkForLC_CTYPE_C
					label:				qsTr("Let JASP guess the best setting for LC_CTYPE (recommended!)")		
					toolTip:			qsTr("Check the install and user directory path for compatibility with LC_CTYPE=\"C\" and set if reasonable.")
					checked:			preferencesModel.lcCtypeWin == 0
					onCheckedChanged:	if(checked) preferencesModel.lcCtypeWin = 0
					KeyNavigation.tab:	alwaysSetLC_CTYPE_C
					KeyNavigation.down:	alwaysSetLC_CTYPE_C
				}
				
				RadioButton
				{
					id:					alwaysSetLC_CTYPE_C
					label:				qsTr("Always set LC_CTYPE to \"C\"")		
					toolTip:			qsTr("See the documentation for more info.")
					info:				qsTr("If this is enabled and you have non-ascii characters in your install path JASP won't work anymore.  If you only have non-ascii characters in your username then installing modules will probably break.")
					checked:			preferencesModel.lcCtypeWin == 1
					onCheckedChanged:	if(checked) preferencesModel.lcCtypeWin = 1
					KeyNavigation.tab:	neverSetLC_CTYPE_C
					KeyNavigation.down:	neverSetLC_CTYPE_C
				}
				
				RadioButton
				{
					id:					neverSetLC_CTYPE_C
					label:				qsTr("Keep LC_CTYPE at system default")		
					toolTip:			qsTr("See the documentation for more info.")
					info:				qsTr("Enabling this will make certain characters in the results look weird, but at least you can use JASP if you installed it in a folder with non-ascii characters in the path. Sorry for the inconvenience, we are working on it and hopefully have this fixed next release.")
					checked:			preferencesModel.lcCtypeWin == 2
					onCheckedChanged:	if(checked) preferencesModel.lcCtypeWin = 2
					KeyNavigation.tab:	logToFile
					KeyNavigation.down:	logToFile
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
				x:					jaspTheme.subOptionOffset
				height:				maxLogFilesSpinBox.height
				width:				showLogs.x + showLogs.width
				enabled:			preferencesModel.logToFile


				SpinBox
				{
					id:					maxLogFilesSpinBox
					value:				preferencesModel.logFilesMax
					onValueChanged:		if(value !== "") preferencesModel.logFilesMax = value
					from:				5 //Less than 5 makes no sense as on release you get 1 for Desktop and 4 from the Engines
					to:					1000000
					defaultValue:		10
					stepSize:			1
					KeyNavigation.tab:	showLogs
					KeyNavigation.down:	showLogs
					text:				qsTr("Max logfiles to keep: ")

					anchors
					{
						leftMargin:	jaspTheme.generalAnchorMargin
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
						margins:	jaspTheme.generalAnchorMargin
						left:		maxLogFilesSpinBox.right
					}


					KeyNavigation.tab:	maxEngineCount
					KeyNavigation.down:	maxEngineCount
				}
			}
		}
		
		PrefsGroupRect
		{
			id:		engineGroup
			title:	qsTr("Engine options")
			
			SpinBox
			{
				id:					maxEngineCount
				value:				preferencesModel.maxEngines
				onValueChanged:		if(value !== "") preferencesModel.maxEngines = value
				from:				1
				to:					16
				defaultValue:		4
				stepSize:			1
				KeyNavigation.tab:	rememberModulesSelected
				KeyNavigation.down:	rememberModulesSelected
				text:				qsTr("Maximum # of engines: ")
			}
			
		}
	}
}
