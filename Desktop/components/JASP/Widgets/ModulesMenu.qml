import QtQuick
import QtQuick.Controls as QTC
import QtQuick.Effects
import QtQuick.Layouts
import JASP.Controls
import QtWebEngine
import QtWebChannel
import "./FileMenu"

FocusScope
{
	id:			modulesMenu

	width:		slidePart.width
	height:		600
	z:			1
	visible:	opened || slideAnimation.running

	property bool opened: false //should be from some model
	property int currentIndex: preferencesModel.developerMode ? -3 : -1  // -2, -3 denote install module and developer mode buttons

	onVisibleChanged: engineSync.activateUtilEngine = visible

	onOpenedChanged: {

		if(!opened) ribbonModel.highlightedModuleIndex = -1; else forceActiveFocus();


	}

	Keys.onEscapePressed:	closeAndFocusRibbon();
	Keys.onRightPressed:	closeAndFocusRibbon();
	Keys.onLeftPressed:		closeAndFocusRibbon();

	function closeAndFocusRibbon()
	{
		opened       = false;
		ribbon.focus = true;
	}

	Keys.onTabPressed:		{ increaseIndex(); }
	Keys.onBacktabPressed:	{ decreaseIndex(); }
	Keys.onDownPressed:		{ increaseIndex(); }
	Keys.onUpPressed:		{ decreaseIndex(); }

	function increaseIndex()
	{
		if (preferencesModel.developerMode)
		{
			let nextIndex  = currentIndex + 1;
			if (nextIndex === repeater.count)
				nextIndex  = -2;
			currentIndex   = nextIndex;
		}
		else
			currentIndex   = mod(currentIndex + 1, repeater.count);
	}

	function decreaseIndex()
	{
		if (preferencesModel.developerMode)
		{
			let nextIndex   = currentIndex - 1;
			if (nextIndex  === -3)
				nextIndex   = repeater.count -1;
			currentIndex    = nextIndex;
		}
		else
			currentIndex = mod(currentIndex - 1, repeater.count);
	}

	onCurrentIndexChanged:
	{
		if (currentIndex < 0)
			vertScroller.scrollToElement(addModuleButton)
		else
			vertScroller.scrollToElement(repeater.itemAt(currentIndex))
	}

	Rectangle
	{
		id:				slidePart
		x:				modulesMenu.opened ? 0 : width
		width:			modulesFlick.width + vertScroller.visibleBreadth + moduleStoreContainer.width + 2 * jaspTheme.contentMargin
		height:			modulesMenu.height
		color:			jaspTheme.fileMenuColorBackground
		border.width:	1
		border.color:	jaspTheme.uiBorder

		Behavior on x { enabled: preferencesModel.animationsOn; PropertyAnimation { id: slideAnimation; duration: jaspTheme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


		MouseArea
		{
			id:				gottaCatchEmAll //Clicks that is
			anchors.fill:	parent
			z:				-6
		}

		ScrollMoreIndicator
		{
			id: 		scrollingGuideBottom

			anchors
			{
				left:			parent.left
				right:			vertScroller.left
				bottom:			parent.bottom
				bottomMargin:	slidePart.border.width
			}

			extraSpace:		modulesFlick.contentHeight - (modulesFlick.contentY + modulesFlick.height)
			visible:		!progressOverlay.visible
		}

		ScrollMoreIndicator
		{
			id:				scrollingGuideTop
			anchors
			{
				left:		parent.left
				right:		vertScroller.left
				top:		parent.top
				topMargin:	slidePart.border.width
			}

			upsideDown:		true
			extraSpace:		modulesFlick.contentY
			visible:		!progressOverlay.visible
		}

		WebEngineProfile {
			id: moduleStoreProfile
			downloadPath: jaspTmpDir

			onDownloadRequested: function(request) {
				console.log("Download requested:", request.url)

				if (moduleStore.downloadInProgress) {
					console.log("Download already in progress, cancelling duplicate request.");
					request.cancel();
					return;
				}

				let name = request.downloadFileName
				let index = name.lastIndexOf('.');
				let extension = index !== -1 ? name.substring(index + 1) : '';
				if(extension === 'JASPModule') {
					// Use translated module name from url hash if available, fallback to filename parsing
					const url = new URL(request.url);
					const hash = url.hash ? url.hash.substring(1) : '';
					const hashParams = new URLSearchParams(hash);
					const translatedName = hashParams.get('t');
					if (translatedName) {
						moduleStore.currentModuleName = translatedName;
					} else {
						index = name.indexOf('_');
						moduleStore.currentModuleName = index !== -1 ? name.substring(0, index) : name;
					}
					moduleStore.isInitiatingDownload = false;
					moduleStore.downloadInProgress = true
					moduleStore.downloadTotal = request.totalBytes
					moduleStore.downloadProgress = Qt.binding(function() { return request.receivedBytes; })
					moduleStore.currentDownloadRequest = request
					request.accept()
				}
				else
					request.cancel()
			}

			onDownloadFinished: function(request) { //All Jasp Store module installs run via this code
				moduleStore.downloadInProgress = false
				moduleStore.currentDownloadRequest = null
				if (request.state !== WebEngineDownloadRequest.DownloadCompleted) {
					console.log("Download interrupted:", request.interruptReasonString)
					moduleStore.isInitiatingDownload = false; //failsafe
					moduleStore.triggerNextDownload()
					return
				}
				console.log("Download finished:", request.downloadFileName)
				let path = request.downloadDirectory + '/' + request.downloadFileName
				moduleLibrary.startInstalling()
				dynamicModules.installJASPModule(path)
			}
		}

		Item
		{
			id:						moduleStoreContainer
			visible:                !ribbonModel.dataMode
			clip:                   true
			width:                  !ribbonModel.dataMode ? 500 * preferencesModel.uiScale : 0
			anchors
			{
				top:				modulesFlick.top
				right:				modulesFlick.left
				bottom:				modulesFlick.bottom
				margins:			jaspTheme.contentMargin
			}

			WebEngineView
			{
				id:						moduleStore
				visible:                preferencesModel.checkUpdates
				anchors.fill:			parent
				url:                    preferencesModel.checkUpdates ? preferencesModel.moduleLibraryURL : "about:blank"
				profile:                moduleStoreProfile
				zoomFactor:             preferencesModel.uiScale

				onNewWindowRequested: (request) =>
				{
					Qt.openUrlExternally(request.requestedUrl);
					request.accept();
				}

				property int _retryCount: 0

				function checkForUpdates() {
					var js = "var updates=[];document.querySelectorAll('a').forEach(function(el){if(el.textContent.trim()==='Update'){var m=el.href.match(/jasp-stats-modules\\/([^\\/]+)\\//);if(m)updates.push(m[1])}});JSON.stringify(updates);";
					runJavaScript(js, function(result) {
						console.log("checkForUpdates result:", result);
						if (result && result.length > 0) {
							var names;
							try {
								names = JSON.parse(result);
							} catch(e) {
								console.log("checkForUpdates: failed to parse result:", e);
								if (_retryCount < 10) {
									_retryCount++;
									_retryTimer.start();
								}
								return;
							}
							console.log("checkForUpdates: updatable =", names);
							moduleLibrary.updatableModuleNames = names;
						} else if (_retryCount < 10) {
							_retryCount++;
							_retryTimer.start();
						}
					});
				}

				property Timer _retryTimer: Timer {
					interval: 500
					repeat: false
				}

				Component.onCompleted: {
					_retryTimer.triggered.connect(checkForUpdates);
				}

				onLoadingChanged: (loadRequest) =>
				{
					if (loadRequest.status === WebEngineView.LoadSucceededStatus && url.toString() !== "about:blank") {
						_retryCount = 0;
						_retryTimer.start();
					}
				}

				property bool	downloadInProgress: false;
				property bool	installInProgress: false;
				property int		downloadProgress;
				property int		downloadTotal;
				property var		currentDownloadRequest: null;

				property bool    isInitiatingDownload: false
				property var     downloadQueue: []
				property bool    isProcessingQueue: false
				property int     batchTotal: 0
				property int     batchCurrent: 0
				property string  currentModuleName: ""

				function triggerNextDownload() {
					if (isInitiatingDownload || downloadInProgress || moduleLibrary.isInstalling) { //To many double triggers of signals to guard against
						return;
					}

					if (downloadQueue.length > 0) {
						isProcessingQueue = true;
						isInitiatingDownload = true;
						batchCurrent++;
						let nextUrl = downloadQueue.shift();
						//little hack so we may process the downloads using the existing code path in WebEngineProfile
						let jsSnippet = "var a = document.createElement('a'); a.href = '" + nextUrl + "'; a.download = ''; document.body.appendChild(a); a.click(); document.body.removeChild(a);";
						runJavaScript(jsSnippet);
					} else {
						isProcessingQueue = false;
						isInitiatingDownload = false;
						batchTotal = 0;
						batchCurrent = 0;
						currentModuleName = "";
					}
				}

				Connections {
					target: moduleLibrary
					function onIsInstallingChanged() {
						if (!moduleLibrary.isInstalling && moduleStore.isProcessingQueue) {
							moduleStore.triggerNextDownload();
						}
					}
					function onRequestModulePageRefresh() {
						if (!moduleStore.isProcessingQueue && !moduleStore.downloadInProgress)
							moduleStore.reloadAndBypassCache();
					}
				}

				webChannel.registeredObjects:	[ moduleStoreWebChannel ]

				QtObject {
					id: moduleStoreWebChannel
					WebChannel.id: "moduleStore"

					function info() {
						return moduleLibrary.getEnvironmentInfo();
					}

					signal environmentInfoChanged(var environmentInfo)

					Component.onCompleted: {
						moduleLibrary.environmentInfoChanged.connect(moduleStoreWebChannel.environmentInfoChanged)
					}
					Component.onDestruction: {
						moduleLibrary.environmentInfoChanged.disconnect(moduleStoreWebChannel.environmentInfoChanged)
					}

					function uninstall(moduleName) {
						moduleLibrary.uninstallJASPModule(moduleName)
					}

					function installMany(asset_urls) { //We fill a queue and trigger first download
						if (!asset_urls || asset_urls.length === 0) return;
						if (!moduleStore.isProcessingQueue && !moduleStore.downloadInProgress && !moduleLibrary.isInstalling) {
							moduleStore.batchTotal = asset_urls.length;
							moduleStore.batchCurrent = 0;
						}

						for (let i = 0; i < asset_urls.length; i++) {
							if (moduleStore.downloadQueue.indexOf(asset_urls[i]) === -1) {
								moduleStore.downloadQueue.push(asset_urls[i]);
							}
						}

						if (!moduleStore.downloadInProgress && !moduleStore.isProcessingQueue) { //les go
							moduleStore.triggerNextDownload();
						}
					}
				}
			}

			Rectangle
			{
				id:					checkUpdatesDisabledMessage
				visible:			!preferencesModel.checkUpdates
				anchors.fill:		parent
				color:				jaspTheme.uiBackground
				border.width:		1
				border.color:		jaspTheme.uiBorder

				Column
				{
					anchors.centerIn:	parent
					anchors.margins:	20 * preferencesModel.uiScale
					width:				parent.width - 40 * preferencesModel.uiScale
					spacing:			10 * preferencesModel.uiScale

					Text
					{
						text:					qsTr("Not allowed to show the module library to install modules")
						width:					parent.width
						wrapMode:				Text.WordWrap
						horizontalAlignment:	Text.AlignHCenter
						font:					jaspTheme.fontGroupTitle
						color:					jaspTheme.textEnabled
					}

					Text
					{
						width:					parent.width
						wrapMode:				Text.WordWrap
						horizontalAlignment: 	Text.AlignHCenter
						text:					qsTr("In \"Preferences\" > \"Interface\" the \"Check for updates\" option is turned off. Please turn it on to see the module library.")
						font:					jaspTheme.font
						color:					jaspTheme.textEnabled
					}
				}
			}
		}


		Flickable
		{
			id:						modulesFlick
			flickableDirection:		Flickable.VerticalFlick
			contentHeight:			workspaceSpecs.visible ? workspaceSpecs.height : modules.height
			contentWidth:			width
			width:                  340 * preferencesModel.uiScale
			clip:					true

			anchors
			{
				top:				parent.top
				margins:			jaspTheme.contentMargin
				right:				vertScroller.visible ? vertScroller.left : parent.right
				bottom:				parent.bottom
			}

			Column
			{
				id:			workspaceSpecs
				spacing:	jaspTheme.rowSpacing
				width:		modulesFlick.width
				visible:	ribbonModel.dataMode

				MenuHeader
				{
					headertext:	qsTr("Workspace settings")
				}


				PrefsGroupRect
				{
					spacing:	jaspTheme.rowSpacing
					width:		parent.width - (jaspTheme.generalAnchorMargin * 2)
					color:		jaspTheme.uiBackground


					Text
					{
						anchors.margins:	3 * preferencesModel.uiScale
						text:				qsTr("Name: %1").arg(workspaceModel.name)
					}

					TextArea
					{
						anchors.margins:	3 * preferencesModel.uiScale
						title:				qsTr("Description: ")
						height:				100 * jaspTheme.uiScale
						control.padding:	3 * jaspTheme.uiScale
						text:				workspaceModel.description
						onEditingFinished: 	if(workspaceModel.description !== text) workspaceModel.description = text
						applyScriptInfo:	""
						placeholderText:	"..."
						undoModel:			columnModel
						useTabAsSpaces:		false
						nextTabItem:		missingValues
						wrapMode:			TextEdit.Wrap
						anchors
						{
							left:			parent.left
							right:			parent.right
							margins:		jaspTheme.generalAnchorMargin
						}

					}
				}

				PrefsMissingValues
				{
					id:								missingValues
					width:							parent.width - (jaspTheme.generalAnchorMargin * 2)
					x:								jaspTheme.generalAnchorMargin
					model:							workspaceModel
					resetButtonTooltip:				qsTr("Reset missing values with the ones set in Data Preferences")
					showWorkspaceMissingValues:		false
				}
			}

			Column
			{
				id:			modules
				spacing:	4  * preferencesModel.uiScale
				width:		modulesFlick.width
				visible:	!ribbonModel.dataMode
				//anchors.right: parent.right //vertScroller.visible ? vertScroller.left : parent.right

				property int buttonMargin:	3  * preferencesModel.uiScale
				property int buttonWidth:	width - (buttonMargin * 2)
				property int buttonHeight:	40  * preferencesModel.uiScale

				MenuButton
				{
					id:					addModuleButton
					text:				qsTr("Install Local Module")
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					onClicked: 			moduleInstallerDialog.open()
					iconSource:			jaspTheme.iconPath + "/install_icon.png"  // icon from https://icons8.com/icon/set/install/cotton
					showIconAndText:	true
					iconLeft:			false
					toolTip:			qsTr("Install a local module")
					visible:			preferencesModel.developerMode
					focus:				currentIndex === -2
					activeFocusOnTab:	false
				}

				QTC.ToolSeparator
				{
					orientation:				Qt.Horizontal
					width:						modules.buttonWidth
					visible:					preferencesModel.developerMode
				}

				MenuButton
				{
					id:					addDeveloperModuleButton
					text:				folderSelected ? (dynamicModules.developersModuleInstallButtonEnabled ? qsTr("Install Developer Module") : qsTr("Installing Developer Module")) : qsTr("Select a Developer Module")
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					onClicked: 			folderSelected ? dynamicModules.installJASPDeveloperModule() : preferencesModel.browseDeveloperFolder()
					toolTip:			folderSelected ? (dynamicModules.developersModuleInstallButtonEnabled ? qsTr("Install selected developer module") : qsTr("Installing developer module now")) : qsTr("Select a developer module by clicking here")
					visible:			preferencesModel.developerMode && !preferencesModel.directLibpathEnabled
					enabled:			dynamicModules.developersModuleInstallButtonEnabled
					focus:				currentIndex === -1
					activeFocusOnTab:	false

					readonly property bool folderSelected: preferencesModel.developerFolder != ""
				}

				MenuButton
				{
					id:					addDeveloperModuleDirectButton
					text:				moduleSelected ? qsTr("Install Developer Module") : qsTr("Select a Developer Module")
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					onClicked: 			moduleSelected ? dynamicModules.installJASPDeveloperModule() : fileMenuModel.showAdvancedPreferences()
					toolTip:			moduleSelected ? qsTr("Install selected developer module") : qsTr("Select a developer module by filling in the relevant preferences")
					visible:			preferencesModel.developerMode && preferencesModel.directLibpathEnabled
					focus:				currentIndex === -1
					activeFocusOnTab:	false

					readonly property bool moduleSelected: preferencesModel.directLibpathEnabled && preferencesModel.directLibpathFolder != "" && preferencesModel.directDevModName != ""
				}

				QTC.ToolSeparator
				{
					orientation:				Qt.Horizontal
					width:						modules.buttonWidth
					visible:					preferencesModel.developerMode
				}

				Repeater
				{
					id:		repeater
					model:	ribbonModelUncommon

					//The row being dragged and where it would end up. The model is only reordered once, when the
					//mouse is released; until then the other rows are slid aside to show the gap it will drop into.
					property int  draggedIndex:		-1
					property int  dropTargetIndex:	-1

					//Turned off just before the model is reordered, so the rows do not slide back from a gap that
					//is about to be filled by the dragged module anyway
					property bool slideRows:			false

					readonly property real rowStep:	modules.buttonHeight + modules.spacing

					//How far a row must slide to open up the gap the dragged module will drop into
					function rowShift(row)
					{
						if(draggedIndex < 0 || dropTargetIndex < 0 || row === draggedIndex)
							return 0;

						if(draggedIndex < dropTargetIndex)	return row >  draggedIndex	&& row <= dropTargetIndex	? -rowStep : 0;
						else								return row >= dropTargetIndex	&& row <  draggedIndex		?  rowStep : 0;
					}

					DropArea
					{
						id:					moduleDropArea
						width:				modules.buttonWidth
						height:				modules.buttonHeight
						keys:				["module"]

						//Dragging over a row marks it as the spot to land on and the rows in between slide over at
						//once, but nothing is reordered until the mouse is released. Special rows (R console and the
						//like) are no target: moveModule would refuse them anyway and they must keep their place.
						//The target is not cleared on exit, so passing over the gaps between rows does not make them
						//slide back and forth.
						onEntered:
						{
							if(ribbonModelUncommon.isModule(index))
								repeater.dropTargetIndex = index
						}

						//Shows where the module will be put down
						Rectangle
						{
							anchors.fill:		parent
							visible:			repeater.dropTargetIndex === index
							color:				"transparent"
							radius:				jaspTheme.borderRadius
							border.color:		jaspTheme.focusBorderColor
							border.width:		2
						}

						Rectangle
						{
							id:					moduleRow
							width:				modules.buttonWidth
							height:				modules.buttonHeight
							anchors.leftMargin: modules.buttonMargin
							color:				isSpecial || dynamicModule.status !== "error" ? "transparent" : jaspTheme.red

							property int myIndex:	index

							//Only the row slides aside, not the drop area around it, so the module being dragged keeps
							//hitting the same rows however far they have moved out of its way
							transform: Translate
							{
								y:	repeater.rowShift(moduleRow.myIndex)

								Behavior on y
								{
									enabled: repeater.slideRows && preferencesModel.animationsOn
									NumberAnimation { duration: 150; easing.type: Easing.OutQuad }
								}
							}

							Drag.keys:			["module"]
							Drag.active:		moduleDragArea.drag.active
							Drag.hotSpot.x:		width  / 2
							Drag.hotSpot.y:		height / 2

							states:
							[
								State
								{
									name:	"dragging"
									when:	moduleRow.Drag.active

									//Out of the column while being dragged, otherwise it is positioned by it and cannot follow the cursor
									ParentChange	{ target: moduleRow; parent: modulesFlick										}
									AnchorChanges	{ target: moduleRow; anchors.top: undefined; anchors.left: undefined			}
									PropertyChanges	{ restoreEntryValues: false; moduleRow { z: 10 }								}
								},

								State
								{
									name:	"chilling"
									when:	!moduleRow.Drag.active

									ParentChange	{ target: moduleRow; parent: moduleDropArea									}
									AnchorChanges	{ target: moduleRow; anchors.top: parent.top; anchors.left: parent.left			}
								}
							]

							RectangularShadow
							{
								anchors.centerIn:	moduleRow
								width:				moduleRow.width
								height:				moduleRow.height
								visible:			moduleRow.Drag.active
								color:				jaspTheme.grayDarker
								blur:				10
								spread:				3
								radius:				jaspTheme.borderRadius
								offset.x:			0
								offset.y:			0
							}




							CheckBox
							{
								id:					moduleButton
								label:				displayText
								checked:			ribbonEnabled
								onCheckedChanged:	ribbonModelUncommon.setModuleEnabled(index, checked)
								enabled:			isSpecial || !(dynamicModule.loading || dynamicModule.installing)
								font:				jaspTheme.fontRibbon
								focus:				index === currentIndex
								forwardKeys:		true
								Keys.forwardTo:		[modulesMenu]

								toolTip:			isSpecial										? qsTr("Ready") //Always ready!
												: dynamicModule.installing						? qsTr("Installing: %1\n").arg(dynamicModule.installLog)
												: dynamicModule.loading							? qsTr("Loading: %1\n").arg(dynamicModule.loadLog)
												: dynamicModule.status === "readyForUse"	? qsTr("Loaded and ready for use!")
												: dynamicModule.status === "error"				? qsTr("Error occurred!")
																														: qsTr("Not ready for use?")

								anchors
								{
									left		: parent.left
									right		: refreshButton.left
									verticalCenter	: parent.verticalCenter
								}
							}

							MenuButton
							{
								z:				1
								id:				refreshButton
								visible:		isDevMod
								iconSource:		jaspTheme.iconPath + "/redo.svg"
								width:			visible ? height : 0
								onClicked:		dynamicModules.refreshDeveloperModule();
								toolTip:		qsTr("Refresh developer module ") + displayText
								anchors
								{
									right			: minusButton.left
									verticalCenter	: parent.verticalCenter
								}
							}

							MenuButton
							{
								z:				1
								id:				minusButton
								visible:		!isBundled && !isSpecial
								iconSource:		hovered ? jaspTheme.iconPath + "/delete_icon.png" : jaspTheme.iconPath + "/delete_icon_gray.png"  // icon from https://icons8.com/icon/set/delete/material
								width:				visible ? height : 0
								onClicked: 			dynamicModules.uninstallJASPModule(moduleName)
								toolTip:			qsTr("Uninstall module ") + displayText
								anchors
								{
									right:			parent.right
									verticalCenter:	parent.verticalCenter
								}
							}

							//On top of the checkbox (so a module can be picked up anywhere on its row) but underneath the
							//refresh and uninstall buttons, which carry a z of their own. A click is only delivered when the
							//press did not turn into a drag, so it can hand the toggle to the model itself.
							MouseArea
							{
								id:					moduleDragArea
								anchors.fill:		parent
								enabled:			!isSpecial
								hoverEnabled:		true
								cursorShape:		moduleRow.Drag.active ? Qt.ClosedHandCursor : Qt.PointingHandCursor
								drag.target:		moduleRow
								drag.axis:			Drag.YAxis

								//Reorder once, on release, rather than every time another row is passed over: moveModule
								//writes the order to the settings, and a drag would otherwise rewrite them all the way over.
								drag.onActiveChanged:
								{
									if(drag.active)
									{
										repeater.draggedIndex		= moduleRow.myIndex
										repeater.dropTargetIndex	= moduleRow.myIndex
										repeater.slideRows			= true
									}
									else
									{
										//Stop sliding before the rows are put back where the column wants them: the model
										//move right after this drops the module into the gap they were holding open.
										repeater.slideRows			= false

										let from					= moduleRow.myIndex
										let to						= repeater.dropTargetIndex

										repeater.draggedIndex		= -1
										repeater.dropTargetIndex	= -1

										if(to >= 0 && to !== from)
											ribbonModelUncommon.moveModule(from, to)
									}
								}

								onClicked:			if(moduleButton.enabled) ribbonModelUncommon.setModuleEnabled(index, !moduleButton.checked)

								QTC.ToolTip.text:		qsTr("Drag to reorder the modules in the ribbon")
								QTC.ToolTip.visible:	containsMouse && !moduleRow.Drag.active && repeater.count > 1
							}
						}
					}
				}
			}



		}

		JASPScrollBar
		{
			id:				vertScroller
			flickable:		modulesFlick
			manualAnchor:	true
			vertical:		true
			anchors
			{
				top:	modulesFlick.top
				right:	parent.right
				bottom:	modulesFlick.bottom
			}
		}

		focus: true

		// Progress bar overlay for download and installation
		Rectangle
		{
			id:				progressOverlay
			anchors
			{
				left:		moduleStoreContainer.left
				right:		moduleStoreContainer.right
				top:		moduleStoreContainer.top
				bottom:		moduleStoreContainer.bottom
			}
			color:			jaspTheme.fileMenuColorBackground
			visible:		moduleStore.downloadInProgress || moduleLibrary.isInstalling || moduleStore.batchTotal > 0
			z:				10
			clip:			true
			property real	waveHeight:		86 * preferencesModel.uiScale
			property real	waveWidth:		1400 * preferencesModel.uiScale

			MouseArea
			{
				anchors.fill: parent
				propagateComposedEvents: false
				hoverEnabled: true
				preventStealing: true
			}

			Image
			{
				id:						overlayTopWave
				z:						1
				opacity:				0.35
				fillMode:				Image.TileHorizontally
				horizontalAlignment:	Image.AlignHCenter
				height:					sourceSize.height
				width:					progressOverlay.width + progressOverlay.waveWidth
				sourceSize.width:		progressOverlay.waveWidth
				sourceSize.height:		progressOverlay.waveHeight
				source:					jaspTheme.iconPath + (!PRO ? "jasp-wave-down-blue-120.svg" : "jasp-wave-down-pro-120.svg")
				cache:					false
				anchors.top:			parent.top

				NumberAnimation on x
				{
					from:		0
					to:			-progressOverlay.waveWidth
					duration:	8000
					loops:		Animation.Infinite
					running:	progressOverlay.visible
				}
			}

			Image
			{
				id:						overlayBottomWave
				z:						1
				opacity:				0.35
				fillMode:				overlayTopWave.fillMode
				horizontalAlignment:	Image.AlignHCenter
				height:					overlayTopWave.height
				width:					progressOverlay.width + progressOverlay.waveWidth
				sourceSize.width:		overlayTopWave.sourceSize.width
				sourceSize.height:		overlayTopWave.sourceSize.height
				source:					jaspTheme.iconPath + (!PRO ? "jasp-wave-up-green-120.svg" : "jasp-wave-up-pro-120.svg")
				cache:					false
				anchors.bottom:			parent.bottom

				NumberAnimation on x
				{
					from:		0
					to:			-progressOverlay.waveWidth
					duration:	8000
					loops:		Animation.Infinite
					running:	progressOverlay.visible
				}
			}

			Column
			{
				z:					2
				anchors.centerIn:	parent
				spacing:			10 * preferencesModel.uiScale
				width:				300 * preferencesModel.uiScale

				Text
				{
					id:					progressText
					text: {
						let name = moduleStore.currentModuleName !== "" ? moduleStore.currentModuleName : qsTr("module");
						let action = moduleStore.downloadInProgress ? qsTr("Downloading") : qsTr("Installing");
						let progress = moduleStore.batchTotal > 0 ? qsTr(" (%1/%2)").arg(moduleStore.batchCurrent).arg(moduleStore.batchTotal) : "";
						return progress + " " + action + " " + name + " ...";
					}
					color:				jaspTheme.black
					font.pixelSize:		16 * preferencesModel.uiScale
					anchors.horizontalCenter: parent.horizontalCenter
				}

				// TODO show progress of installation
				Rectangle
				{
					id:				progressBarBackground
					width:			parent.width
					height:			30 * preferencesModel.uiScale
					color:			jaspTheme.grayDarker
					border.color:	jaspTheme.uiBorder
					border.width:	1
					radius:			3
					visible:		moduleStore.downloadInProgress

					Rectangle
					{
						id:		progressBarFill
						width:	moduleStore.downloadTotal > 0 ? (parent.width * moduleStore.downloadProgress / moduleStore.downloadTotal) : 0
						height:	parent.height
						color:	jaspTheme.blue
						radius:	parent.radius

						Behavior on width
						{
							enabled: preferencesModel.animationsOn
							PropertyAnimation { duration: 100 }
						}
					}

					Text
					{
						anchors.centerIn:	parent
						text:				moduleStore.downloadTotal > 0 ? Math.round((moduleStore.downloadProgress / moduleStore.downloadTotal) * 100) + "%" : "0%"
						color:				jaspTheme.black
						font.pixelSize:		12 * preferencesModel.uiScale
					}
				}

				RoundedButton
				{
					id:					cancelButton
					text:				qsTr("Cancel")
					width:				120 * preferencesModel.uiScale
					height:				30 * preferencesModel.uiScale
					anchors.horizontalCenter: parent.horizontalCenter
					// TODO also allow to cancel installation
					visible:			moduleStore.downloadInProgress

					onClicked:
					{
						if (moduleStore.currentDownloadRequest !== null) {
							moduleStore.currentDownloadRequest.cancel()
						}
						moduleStore.downloadInProgress = false
						moduleStore.currentDownloadRequest = null
					}
					toolTip:			qsTr("Cancel download")
				}
			}
		}

		Item
		{
			id:			dropShadow
			y:			0
			x:			-width
			height:		parent.height
			width:		jaspTheme.shadowRadius

			Rectangle
			{
				anchors.centerIn: parent
				rotation:	90
				gradient:	Gradient {
					GradientStop { position: 0.0; color: jaspTheme.shadow }
					GradientStop { position: 1.0; color: "transparent" } }
				height:		dropShadow.width
				width:		dropShadow.height
			}
		}
	}
}
