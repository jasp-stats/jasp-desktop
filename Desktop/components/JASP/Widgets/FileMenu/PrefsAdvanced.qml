import QtQuick
import JASP.Widgets
import JASP.Controls

PrefsScrollView
{
	id:						scrollPrefs

	MenuHeader
	{
		id:				menuHeader
		headertext:		qsTr("Advanced Preferences")
		helpMD:			allHelp.PrefsAdvanced
		addMargin:		false
	}


	PrefsGroupRect
	{
		title:				qsTr("Modules options")

		CheckBox
		{
			id:					rememberModulesSelected
			enabled:			!preferencesModel.settingIsForced("modulesRemember")
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
				enabled:			!preferencesModel.settingIsForced("cranRepositoryURL")

				text:				preferencesModel.cranRepoURL
				onEditingFinished:	preferencesModel.cranRepoURL = text
				nextEl:				githubPatDefault

				height:				directLibPathLabel.height
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
			enabled:			!preferencesModel.settingIsForced("GITHUB_PAT_UseDefault")
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
				enabled:			!preferencesModel.settingIsForced("GITHUB_PAT_Custom")

				text:				preferencesModel.githubPatCustom
				onEditingFinished:	preferencesModel.githubPatCustom = text

				nextEl:				developerMode

				height:				directLibPathLabel.height
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
			enabled:			!preferencesModel.settingIsForced("developerMode")
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
			enabled:			preferencesModel.developerMode && !preferencesModel.settingIsForced("generateMarkdownHelp")
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
				enabled:			!preferencesModel.settingIsForced("moduleLibraryURL")

				text:				preferencesModel.moduleLibraryURL
				onEditingFinished:	preferencesModel.moduleLibraryURL = text
				nextEl:				cleanModulesFolder

				height:				directLibPathLabel.height
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

			KeyNavigation.tab:		directLibPathLabel
			activeFocusOnTab:		true
		}
	}


	PrefsGroupRect
	{
		id:					editDeveloperFolder
		title:				qsTr("Development module")
		visible:			preferencesModel.developerMode
		enabled:			preferencesModel.developerMode

		Item
		{
			id:					directLibpath
			width:				parent.width
			height:				cranRepoUrl.height

			RoundedButton
			{
				id:						directLibPathLabel
				enabled:				!preferencesModel.settingIsForced("directLibpathFolder")
				text:					qsTr("Project library:")
				width:					Math.max(directDevModName.implicitWidth, directLibPathLabel.implicitWidth)
				onClicked:				preferencesModel.browseDeveloperLibPathFolder()
				activeFocusOnTab:		true
				KeyNavigation.tab:		directLibpathFolder.textInput
				KeyNavigation.backtab:	cleanModulesFolder

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
				enabled:			!preferencesModel.settingIsForced("directLibpathFolder")

				text:				preferencesModel.directLibpathFolder
				onEditingFinished:	preferencesModel.directLibpathFolder = text

				nextEl:				moduleName

				height:				directLibPathLabel.height
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
				enabled:			!preferencesModel.settingIsForced("directDevModName")

				text:				preferencesModel.directDevModName
				onEditingFinished:	preferencesModel.directDevModName = text

				nextEl:				useConf

				height:				directLibPathLabel.height
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
			enabled:			!preferencesModel.settingIsForced("useConfigurationFile")
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
				enabled:			!preferencesModel.settingIsForced("remoteConfiguration")
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
					enabled:			!preferencesModel.settingIsForced("remoteConfigurationURL")

					text:				preferencesModel.remoteConfigurationURL
					onEditingFinished:	preferencesModel.remoteConfigurationURL = text

					height:				directLibPathLabel.height
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
					enabled:			!preferencesModel.settingIsForced("localConfigurationPath")
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
					enabled:			!preferencesModel.settingIsForced("localConfigurationPath")

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
			enabled:			!preferencesModel.settingIsForced("logToFile")
			label:				qsTr("Log to file")
			checked:			preferencesModel.logToFile
			onCheckedChanged:	preferencesModel.logToFile = checked
			toolTip:			qsTr("To store debug-logs of JASP in a file, check this box.")

			KeyNavigation.tab:		maxLogFilesSpinBox
			columns:			2

			SpinBox
			{
				id:					maxLogFilesSpinBox
				enabled:			!preferencesModel.settingIsForced("logFilesMax")
				value:				preferencesModel.logFilesMax
				onValueChanged:		if(value !== "") preferencesModel.logFilesMax = value
				from:				5 //Less than 5 makes no sense as on release you get 1 for Desktop and 4 from the Engines
				to:					1000000
				defaultValue:		10
				stepSize:			1

				KeyNavigation.tab:	showLogs
				text:				qsTr("Max logfiles to keep: ")
			}

			RoundedButton
			{
				id:			showLogs
				text:		qsTr("Show logs")
				onClicked:	mainWindow.showLogFolder();

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
			enabled:			!preferencesModel.settingIsForced("maxEngineCount")
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
			visible:			Qt.platform.os === "windows" || Qt.platform.os === "osx"
			enabled:			(Qt.platform.os === "windows" || Qt.platform.os === "osx") && !preferencesModel.settingIsForced("engineSandbox")
			label:				qsTr("Sandbox engines")
			checked:			preferencesModel.engineSandbox
			onCheckedChanged:	preferencesModel.engineSandbox = checked
			toolTip:			qsTr("Strengthen security by isolating Engines running R-code from the network (and on Windows also from the rest of the filesystem)")

			KeyNavigation.tab:	browseEngineSandboxDirButton
		}

		Item
		{
			id:				engineSandboxDirItem
			width:			parent.width
			height:			browseEngineSandboxDirButton.height
			visible:			Qt.platform.os === "windows"
			enabled:			Qt.platform.os === "windows"

			RoundedButton
			{
				id:						browseEngineSandboxDirButton
				text:				qsTr("Sandbox folder:")
				onClicked:			preferencesModel.browseEngineSandboxDir()
				anchors.left:		parent.left
				anchors.leftMargin:	jaspTheme.subOptionOffset
				toolTip:				qsTr("Browse to the folder used as home for the sandboxed Engines. This is where JASP stores logs, clipboard images and where sandboxed Engines may read and write files. Leave empty to use the default: JASP_Sandbox in your home-folder (chosen over Documents because that one tends to be synced by OneDrive).")

				KeyNavigation.tab:		engineSandboxDirText.textInput
				activeFocusOnTab:	true
			}

			PrefsTextInput
			{
				id:					engineSandboxDirText

				text:				preferencesModel.engineSandboxDir
				onEditingFinished:	preferencesModel.engineSandboxDir = text
				nextEl:				showEnginesWindow
				toolTip:				qsTr("The folder used as home for the sandboxed Engines. Leave empty to use the default: JASP_Sandbox in your home-folder.")

				height:				browseEngineSandboxDirButton.height
				anchors
				{
					left:			browseEngineSandboxDirButton.right
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
				}
			}
		}

		RoundedButton
		{
			id:				showEnginesWindow
			text:			qsTr("Show engines")
			onClicked:			mainWindow.showEnginesWindow()
			activeFocusOnTab:	true
		}
	}

	PrefsGroupRect
	{
		title: qsTr("Remote control")

		CheckBox
		{
			id:					rpcServerEnabled
			enabled:			!preferencesModel.settingIsForced("rpcServerEnabled")
			label:				qsTr("Enable JASP-RPC server")
			checked:			preferencesModel.rpcServerEnabled
			onCheckedChanged:		preferencesModel.rpcServerEnabled = checked
			toolTip:			qsTr("Allow external applications to control JASP via HTTP RPC.")
			KeyNavigation.tab:		rpcServerIp
		}

		Item
		{
			width:				parent.width
			height:				rpcServerIp.height
			enabled:			preferencesModel.rpcServerEnabled

			Label
			{
				text:			qsTr("IP address:")
				anchors
				{
					left:			parent.left
					verticalCenter:	parent.verticalCenter
					margins:		jaspTheme.generalAnchorMargin
				}
			}

			PrefsTextInput
			{
				id:					rpcServerIp
				enabled:			!preferencesModel.settingIsForced("rpcServerIp")
				text:				preferencesModel.rpcServerIp
				onEditingFinished:		preferencesModel.rpcServerIp = text
				nextEl:				rpcServerPort
				anchors
				{
					left:			parent.left
					leftMargin:		80 * preferencesModel.uiScale
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
				}
				KeyNavigation.tab:	rpcServerPort
			}
		}

		Item
		{
			width:				parent.width
			height:				rpcServerPort.height
			enabled:			preferencesModel.rpcServerEnabled

			Label
			{
				text:			qsTr("Port:")
				anchors
				{
					left:			parent.left
					verticalCenter:	parent.verticalCenter
					margins:		jaspTheme.generalAnchorMargin
				}
			}

			PrefsTextInput
			{
				id:					rpcServerPort
				enabled:			!preferencesModel.settingIsForced("rpcServerPort")
				text:				preferencesModel.rpcServerPort
				onEditingFinished:		preferencesModel.rpcServerPort = parseInt(text)
				anchors
				{
					left:			parent.left
					leftMargin:		80 * preferencesModel.uiScale
					right:			parent.right
					margins:		jaspTheme.generalAnchorMargin
				}
			}
		}
	}
}
