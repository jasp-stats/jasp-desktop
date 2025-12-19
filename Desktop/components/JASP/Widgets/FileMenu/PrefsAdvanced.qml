import QtQuick
import QtQuick.Controls
import JASP.Widgets
import JASP.Controls

PrefsScrollView
{
	id:						scrollPrefs
	
	Column
	{
		width:			scrollPrefs.width
		spacing:		jaspTheme.rowSpacing

		MenuHeader
		{
			id:				menuHeader
			headertext:		qsTr("Advanced Preferences")
			helpfile:		"preferences/PrefsAdvanced"
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
				focus:				true
				KeyNavigation.tab:	cranRepoUrl
			}

			Item
			{
				id:		cranRepoUrlItem
				width:	parent.width
				height:	cranRepoUrl.height

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

				PrefsTextInput
				{
					id:					cranRepoUrl

					text:				preferencesModel.cranRepoURL
					onEditingFinished:	preferencesModel.cranRepoURL = text
					nextEl:				githubPatDefault

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			cranRepoUrlLabel.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	githubPatDefault
				}
			}

			CheckBox
			{
				id:					githubPatDefault
				label:				qsTr("Use default PAT for Github")
				checked:			preferencesModel.githubPatUseDefault
				onCheckedChanged:	preferencesModel.githubPatUseDefault = checked
				toolTip:			qsTr("Either use the bundled GITHUB_PAT or, if available, use the one set in environment variables.")

				KeyNavigation.tab:		githubPatCustomToken
			}

			Item
			{
				id:			githubPatCustomTokenItem
				width:		parent.width
				height:		cranRepoUrl.height
				enabled:	!preferencesModel.githubPatUseDefault

				Label
				{
					id:					githubPatCustomLabel
					text:				qsTr("Private GITHUB_PAT:")

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						leftMargin:		jaspTheme.subOptionOffset
					}
				}

				PrefsTextInput
				{
					id:					githubPatCustomToken

					text:				preferencesModel.githubPatCustom
					onEditingFinished:	preferencesModel.githubPatCustom = text

					nextEl:				developerMode

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			githubPatCustomLabel.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					textInput.echoMode:	TextInput.Password

					KeyNavigation.tab:		developerMode
				}
			}

			CheckBox
			{
				id:					developerMode
				label:				qsTr("Developer mode")
				checked:			preferencesModel.developerMode
				onCheckedChanged:	preferencesModel.developerMode = checked
				toolTip:			qsTr("To use JASP Modules enable this option.")
				
				KeyNavigation.tab:	generateMarkdown
			}

			CheckBox
			{
				id:					generateMarkdown
				label:				qsTr("Generate markdown files for help")
				toolTip:			qsTr("Enabling this will generate markdown helpfile from the info at qml options.")
				checked:			preferencesModel.generateMarkdown
				onCheckedChanged:	preferencesModel.generateMarkdown = checked
				visible:			preferencesModel.developerMode
				enabled:			preferencesModel.developerMode
				KeyNavigation.tab:	moduleLibraryUrl

			}

			Item
			{
				id:			moduleLibraryUrlItem
				width:		parent.width
				height:		moduleLibraryUrl.height
				visible:	preferencesModel.developerMode
				enabled:	preferencesModel.developerMode

				Label
				{
					id:			moduleLibraryUrlLabel
					text:		qsTr("Module library URL: ")

					anchors
					{
						left:			parent.left
						verticalCenter: parent.verticalCenter
						margins:		jaspTheme.generalAnchorMargin
					}
				}

				PrefsTextInput
				{
					id:					moduleLibraryUrl

					text:				preferencesModel.moduleLibraryURL
					onEditingFinished:	preferencesModel.moduleLibraryURL = text
					nextEl:				cleanModulesFolder

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:		moduleLibraryUrlLabel.right
						right:		parent.right
						margins:	jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	cleanModulesFolder
				}
			}
	
			RoundedButton
			{	
				id:					cleanModulesFolder
				text:				qsTr("Clear installed modules and packages")
                toolTip:			qsTr("This will erase the 'renv', 'Modules' and development Module folders in the appdata.")
				onClicked:			mainWindow.clearModulesFoldersUser();

				KeyNavigation.tab:		directLibpathDevModEnabled
				activeFocusOnTab:		true
			}
		}

		
		PrefsGroupRect
		{
			id:					editDeveloperFolder
			title:				qsTr("Development module")
			visible:			preferencesModel.developerMode
			enabled:			preferencesModel.developerMode

			CheckBox
			{
				id:					directLibpathDevModEnabled
				label:				qsTr("Enable renv mode") //We should really remove the old way and this checkbox
				checked:			preferencesModel.directLibpathEnabled
				onCheckedChanged:	preferencesModel.directLibpathEnabled = checked
				toolTip:			qsTr("Load modules from a binary in an R-library instead of installing it from sources.")
				visible:			preferencesModel.developerMode

				KeyNavigation.tab:	browseDeveloperFolderButton
			}


			Item
			{
				width:				parent.width
				height:				browseDeveloperFolderButton.height
				enabled:			preferencesModel.developerMode && !preferencesModel.directLibpathEnabled
				visible:			preferencesModel.developerMode && !preferencesModel.directLibpathEnabled


				RoundedButton
				{
					id:						browseDeveloperFolderButton
					text:					qsTr("Source folder:")
					onClicked:				preferencesModel.browseDeveloperFolder()
					anchors.left:			parent.left
					anchors.leftMargin:		jaspTheme.subOptionOffset
					toolTip:				qsTr("Browse to your JASP Module folder.")

					KeyNavigation.tab:		developerFolderText.textInput
					activeFocusOnTab:		true

				}

				PrefsTextInput
				{
					id:					developerFolderText

					text:				preferencesModel.developerFolder
					onEditingFinished:	preferencesModel.developerFolder = text
					nextEl:				directLibPathLabel

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			browseDeveloperFolderButton.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}
				}
			}

			Item
			{
				id:					directLibpath
				enabled:			preferencesModel.directLibpathEnabled
				visible:			preferencesModel.developerMode && preferencesModel.directLibpathEnabled
				width:				parent.width
				height:				cranRepoUrl.height

				RoundedButton
				{
					id:						directLibPathLabel
					text:					qsTr("Project library:")
					width:					Math.max(directDevModName.implicitWidth, directLibPathLabel.implicitWidth)
					onClicked:				preferencesModel.browseDeveloperLibPathFolder()
					activeFocusOnTab:		true
					KeyNavigation.tab:		directLibpathFolder.textInput
					KeyNavigation.backtab:	directLibpathDevModEnabled

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						leftMargin:		jaspTheme.subOptionOffset
					}


				}

				PrefsTextInput
				{
					id:					directLibpathFolder

					text:				preferencesModel.directLibpathFolder
					onEditingFinished:	preferencesModel.directLibpathFolder = text

					nextEl:				moduleName

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			directLibPathLabel.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	moduleName

					toolTip:			qsTr("Choose the R library where you installed the development module")
				}
			}

			Item {

				id:					directDevMod
				enabled:			preferencesModel.developerMode &&preferencesModel.directLibpathEnabled
				visible:			preferencesModel.developerMode && preferencesModel.directLibpathEnabled
				width:				parent.width
				height:				cranRepoUrl.height

				Label
				{
					id:					directDevModName
					text:				qsTr("Module name:")
					width:				Math.max(directDevModName.implicitWidth, directLibPathLabel.implicitWidth)

					anchors
					{
						left:			parent.left
						verticalCenter:	parent.verticalCenter
						leftMargin:		jaspTheme.subOptionOffset
					}
				}

				PrefsTextInput
				{
					id:					moduleName

					text:				preferencesModel.directDevModName
					onEditingFinished:	preferencesModel.directDevModName = text

					nextEl:				useConf

					height:				browseDeveloperFolderButton.height
					anchors
					{
						left:			directDevModName.right
						right:			parent.right
						margins:		jaspTheme.generalAnchorMargin
					}

					KeyNavigation.tab:	useConf
					toolTip:			qsTr("Enter the (package)name of the development module you want to load")
				}
			}
		}
		
		PrefsGroupRect
		{
			title:				qsTr("Configuration file options")

			CheckBox
			{
				id:					useConf
				label:				qsTr("Use a configuration file.")
				checked:			preferencesModel.useConfigurationFile
				onCheckedChanged:	preferencesModel.useConfigurationFile = checked
				toolTip:			qsTr("Use a configuration file.")

				KeyNavigation.tab:		useRemoteConf
			}

			Column  {
				visible:	preferencesModel.useConfigurationFile
				width:		parent.width
				spacing:	jaspTheme.rowSpacing

				CheckBox
				{
					id:					useRemoteConf
					label:				qsTr("Use remote configuration file.")
					checked:			preferencesModel.remoteConfiguration
					onCheckedChanged:	preferencesModel.remoteConfiguration = checked
					toolTip:			qsTr("Use the remote configuration file pointed to by URL")

					KeyNavigation.tab:		remoteConfURL
				}

				Item
				{
					id:		remoteConfItem
					width:	parent.width
					height:	cranRepoUrl.height
					enabled: preferencesModel.remoteConfiguration

					Label
					{
						id:		remoteSettingsLabel
						text:	qsTr("Configuration URL: ")
						width:	Math.max(remoteSettingsLabel.implicitWidth, browseLocalconfButton.implicitWidth)

						anchors
						{
							left:			parent.left
							verticalCenter:	parent.verticalCenter
						}
					}

					PrefsTextInput
					{
						id:					remoteConfURL

						text:				preferencesModel.remoteConfigurationURL
						onEditingFinished:	preferencesModel.remoteConfigurationURL = text

						height:				browseDeveloperFolderButton.height
						anchors
						{
							left:			remoteSettingsLabel.right
							right:			parent.right
							margins:		jaspTheme.generalAnchorMargin
						}

						KeyNavigation.tab:	localconf
					}
				}

				Item
				{
					id:					localconf
					//enabled:			!preferencesModel.remoteConfiguration
					width:				parent.width
					height:				browseLocalconfButton.height

					RoundedButton
					{
						id:					browseLocalconfButton
						width:				Math.max(remoteSettingsLabel.implicitWidth, browseLocalconfButton.implicitWidth)
						text:				qsTr("Select configuration file")
						onClicked:			preferencesModel.browseConfigurationFile()
						anchors.left:		parent.left
						toolTip:			qsTr("Select configuration file.")

						KeyNavigation.tab:		browseLocalconfFolderText.textInput
						activeFocusOnTab:		true
					}

					PrefsTextInput
					{
						id:					browseLocalconfFolderText

						text:				preferencesModel.localConfigurationPATH
						onEditingFinished:	preferencesModel.localConfigurationPATH = text
						nextEl:				logToFile

						height:				browseLocalconfButton.height
						anchors
						{
							left:			browseLocalconfButton.right
							right:			parent.right
							margins:		jaspTheme.generalAnchorMargin
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

				KeyNavigation.tab:		maxLogFilesSpinBox
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
					text:				qsTr("Max logfiles to keep: ")

					anchors
					{
						leftMargin:	jaspTheme.generalAnchorMargin
						left:		parent.left
						top:		showLogs.top
						bottom:		showLogs.bottom
					}
				}

				RoundedButton
				{
					id:			showLogs
					text:		qsTr("Show logs")
					onClicked:	mainWindow.showLogFolder();
					anchors
					{
						margins:	jaspTheme.generalAnchorMargin
						left:		maxLogFilesSpinBox.right
					}

					KeyNavigation.tab:		maxEngineCount
					activeFocusOnTab:		true
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
				onValueChanged:		if(value != "") preferencesModel.maxEngines = value
				from:				1
				to:					preferencesModel.maxEnginesAdmin > 0 ? preferencesModel.maxEnginesAdmin : 16
				defaultValue:		Math.max(preferencesModel.maxEnginesAdmin, 4)
				stepSize:			1

				KeyNavigation.tab:	engineSandbox
				activeFocusOnTab:			true
				text:				qsTr("Maximum number of engines: ")
			}

			CheckBox
			{
				id:					engineSandbox
				visible:			Qt.platform.os === "windows"
				enabled:			Qt.platform.os === "windows"
				label:				qsTr("Sandbox engines")
				checked:			preferencesModel.engineSandbox
				onCheckedChanged:	preferencesModel.engineSandbox = checked
				toolTip:			qsTr("Strengthen security on Windows by isolating Engines running R-code")

				KeyNavigation.tab:		showEnginesWindow
			}

			RoundedButton
			{
				id:					showEnginesWindow
				text:				qsTr("Show engines")
				onClicked:			mainWindow.showEnginesWindow()
				activeFocusOnTab:		true
			}
		}
	}
}
