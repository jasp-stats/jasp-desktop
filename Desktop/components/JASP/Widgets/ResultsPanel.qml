import QtQuick
import QtWebEngine
import QtWebChannel
import JASP
import QtQuick.Controls
import JASP.Controls		as JC

Rectangle
{
	id:						resultsPane	
	color:					analysesModel.currentAnalysisIndex !== -1 ? jaspTheme.uiBackground : jaspTheme.white

	JC.ALTNavigation.enabled:				true
	JC.ALTNavigation.requestedPostfix:		"R"
	JC.ALTNavigation.onTagMatch:			{ resultsView.nextItemInFocusChain().forceActiveFocus(); }

	Rectangle
	{
		id:						searchRectangle
		height:					searchBar.height + 2*jaspTheme.generalAnchorMargin
		z:						1
		anchors
		{
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
		}
		visible:			false
		color:				jaspTheme.uiBackground 
		border.color:		jaspTheme.uiBorder
		border.width:		1
		
		JC.TextField
		{
			id:						searchBar
			placeholderText:		qsTr("Search in results:")
			value:					""
			Keys.onEscapePressed:	searchRectangle.visible = false
			Keys.onReturnPressed:	search(displayValue)
			onValueChanged:			search(displayValue)
			onDisplayValueChanged:	search(displayValue)
			onVisibleChanged:		search("")
			moveFocusOnEdit:		false
			
			function startSearching()
			{
				searchRectangle.visible = true
				forceActiveFocus();
			}

			Shortcut 
			{
				sequences:		["Ctrl+F"]
				onActivated: 	searchBar.startSearching()
			}

			function search(thisText) { if(resultsView) resultsView.findText(thisText); }
			
			anchors
			{
				left:			parent.left
				right:			closeButton.left
				bottom:			parent.bottom
				margins:		jaspTheme.generalAnchorMargin
			}
			fieldWidth:				parent.width - (jaspTheme.scrollbarBoxWidthBig + closeButton.width)
		}

		JC.MenuButton
		{
			id:					closeButton
			anchors
			{
				right:			searchRectangle.right
				verticalCenter: searchBar.verticalCenter
				margins:		jaspTheme.generalAnchorMargin
			}
			height:				searchBar.height
			width:				height
			iconSource:			jaspTheme.iconPath + "close-button.png"
			onClicked:			searchRectangle.visible = false
			toolTip:			qsTr("Close search bar")
			radius:				height
		}
	}

	WebEngineView
	{
		id:						resultsView
		clip:                   true
		anchors.fill:			parent
		anchors.leftMargin:		1

		url:					resultsJsInterface.resultsPageUrl

		onContextMenuRequested: (request) => request.accepted = true

		backgroundColor:		jaspTheme.uiBackground

		Keys.onPressed: (event) =>
		{
			switch(event)
			{
			case Qt.Key_PageDown:	resultsView.runJavaScript("windows.pageDown();");	event.accepted=true; break;
			case Qt.Key_PageUp:		resultsView.runJavaScript("windows.pageUp();");		event.accepted=true; break;
			}
		}

		property var urlWhitelist : ['www.youtube.com', 'www.youtu.be'] //'*.vimeo.com', '*.vimeocdn.com', '*.akamaized.net', '*.bilibili.com', '*.hdslb.com', '*.qq.com', '*.smtcdns.com'];

		function isURLInWhitelist(hostname)
		{
			for (var i = 0; i < urlWhitelist.length; i++)
			{
				var pattern = urlWhitelist[i];
				if (pattern === hostname)
				{
					return true;
				} 
				else if (pattern.indexOf('*') !== -1)
				{
					var regex = new RegExp(`^${pattern.replace(/\./g, '\\.').replace(/\*/g, '.*')}$`);
					if (regex.test(hostname))
					{
						return true;
					}
				}
			}
			return false;
		}

		onFullScreenRequested: function(request) 
		{
			request.accept()

			if(request.toggleOn)
			{
				analysesModel.visible = false
				minimizeDataPanel()
			}
		}

		onNavigationRequested: (request)=>
		{
			var requestedURL = new URL(request.url);

			if(request.navigationType === WebEngineNavigationRequest.ReloadNavigation || request.url == resultsJsInterface.resultsPageUrl)
			{
				request.accept()
			}
			else if(request.navigationType === WebEngineNavigationRequest.LinkClickedNavigation)
			{
				Qt.openUrlExternally(request.url);
				request.reject();
			}
			else if(isURLInWhitelist(requestedURL.hostname))
			{
				request.accept();
				console.log("Navigation requested accepted:", requestedURL.hostname)
			}
			else
			{
				request.reject();
				console.log("Navigation requested rejected:", requestedURL.hostname)
			}
		}

		onLoadingChanged: (loadRequest)=>
		{
			resultsJsInterface.resultsLoaded = loadRequest.status === WebEngineView.LoadSucceededStatus;
			setTranslatedResultsString();
			if(resultsJsInterface.resultsLoaded)
				runJavaScript(`window.sendUrlWhitelist(${JSON.stringify(urlWhitelist)})`); //sent urlWhitelist to js side
		}



		Connections
		{
			target:		resultsJsInterface
			function onRunJavaScriptSignal(js)			{ resultsView.runJavaScript(js); }
			function onScrollAtAllChanged(scrollAtAll)	{ resultsView.runJavaScript("window.setScrollAtAll("+(scrollAtAll ? "true" : "false")+")"); }

			function onExportToPDF(pdfPath)
			{
				resultsView.printToPdf(pdfPath, preferencesModel.pdfPageSize, preferencesModel.pdfLandscape ? WebEngineView.Landscape : WebEngineView.Portrait);
			}

			function onPrepForExport()
			{
				//set light theme and unselect
				resultsView.runJavaScript("window.unselect(); window.setTheme(\"lightTheme\");", function() { resultsJsInterface.exportPrepFinished(); });
			}
		}
		onPdfPrintingFinished: (filePath)=>
		{
			if(preferencesModel.currentThemeName !== "lightTheme")
				resultsJsInterface.setThemeCss(preferencesModel.currentThemeName);

			resultsJsInterface.pdfPrintingFinished(filePath);
		}

		webChannel.registeredObjects:	[ resultsJsInterfaceInterface ]

		property string resultsString:	qsTr("Results")
		onResultsStringChanged:			setTranslatedResultsString();

		// Defined the elements that need to be translated in the html/js interface
		// where use i18n(...) to return  translations
		property var i18nObject: 
		{
			"Bold":					qsTr("Bold"),					"Italic":			qsTr("Italic"),			"Underline":		qsTr("Underline"),			"Link":						qsTr("Link"),					
			"Formula":				qsTr("Formula"),				"Font Color":		qsTr("Font Color"),		"Add Indent":		qsTr("Add Indent"),			"Embed web video" :			qsTr("Embed web video"), 
			"Code Block":			qsTr("Code Block"),				"Header":			qsTr("Header"),			"Ordered List":		qsTr("Ordered List"),		"Unordered List":			qsTr("Unordered List"),			
			"Background Color":		qsTr("Background Color"),		"Subscript":		qsTr("Subscript"),		"Superscript":		qsTr("Superscript"),		"Blockquote":				qsTr("Blockquote"),				
			"Remove Indent":		qsTr("Remove Indent"),			"Font Size":		qsTr("Font Size"),		"Clear Formatting":	qsTr("Clear Formatting"),	"Click here to add text" :	qsTr("Click here to add text"),
			"Copied to clipboard":	qsTr("Copied to clipboard"),	"Introduction:":	qsTr("Introduction:"),	"Conclusion:" :		qsTr("Conclusion:"),		"Image" :					qsTr("Image"),					
			
			"Unsupported video services":			qsTr("Unsupported video services"),			"Input LaTeX here:":				qsTr("Input LaTeX here:"),		
			"Press `Cmd/Ctrl + Enter` to apply;":	qsTr("Press `Cmd/Ctrl + Enter` to apply;"),	"Click to apply formula":			qsTr("Click to apply formula"), 
			"Click to edit this formula":			qsTr("Click to edit this formula"),			"Citations copied to clipboard":	qsTr("Citations copied to clipboard"), 	
			"LaTeX code copied to clipboard":		qsTr("LaTeX code copied to clipboard"),		"Remove this note":					qsTr("Remove this note"),
			
			"JASP only allows the following videoservices:":								qsTr("JASP only allows the following videoservices:"), 
			"Contact the JASP team to request adding another videoservice to the list." :	qsTr("Contact the JASP team to request adding another videoservice to the list.")
			
		}

		function setTranslatedResultsString()
		{
			if(resultsJsInterface.resultsLoaded)
			{
				runJavaScript("window.setAnalysesTitle(\"" + resultsString + "\");");

				// To parse the QML i18n object to js object
				// will also make the js i18n follow changes of JASP GUI language
				runJavaScript(`window.setI18nStrings(${JSON.stringify(i18nObject)})`) 
			}
		}

		QtObject
		{
			id:				resultsJsInterfaceInterface
			WebChannel.id:	"jasp"

			property bool reportingVisible: preferencesModel.reportingMode

			onReportingVisibleChanged: mainWindow.reloadResults()

			// Yeah I know this "resultsJsInterfaceInterface" looks a bit stupid but this honestly seems like the best way to make the current resultsJsInterface functions available to javascript without rewriting (more of) the structure of Desktop right now.
			// It would be much better to have resultsJsInterface be passed directly though..
			// It also gives you an overview of the functions used in results html

			function openFileTab()								{ resultsJsInterface.openFileTab()                              }
			function saveTextToFile(fileName, html)				{ resultsJsInterface.saveTextToFile(fileName, html)             }
			function analysisUnselected()						{ resultsJsInterface.analysisUnselected()                       }
			function analysisSelected(id)						{ resultsJsInterface.analysisSelected(id)                       }
			function analysisChangedDownstream(id, model)		{ resultsJsInterface.analysisChangedDownstream(id, model)       }
			function analysisTitleChangedInResults(id, title)	{ resultsJsInterface.analysisTitleChangedInResults(id, title)	}
			function analysisSaveImage(id, options)				{ resultsJsInterface.analysisSaveImage(id, options)				}
			function analysisEditImage(id, options)				{ resultsJsInterface.analysisEditImage(id, options)				}
			function removeAnalysisRequest(id)					{ resultsJsInterface.removeAnalysisRequest(id)					}
			function pushToClipboard(mime, raw, coded)			{ resultsJsInterface.pushToClipboard(mime, raw, coded)			}
			function pushImageToClipboard(raw, coded)			{ resultsJsInterface.pushImageToClipboard(raw, coded)			}
			function saveTempImage(index, path, base64)			{ resultsJsInterface.saveTempImage(index, path, base64)			}
			function getImageInBase64(index, path)				{ resultsJsInterface.getImageInBase64(index, path)				}
			function resultsDocumentChanged()					{ resultsJsInterface.resultsDocumentChanged()					}
			function displayMessageFromResults(msg)				{ resultsJsInterface.displayMessageFromResults(msg)				}
			function setAllUserDataFromJavascript(json)			{ resultsJsInterface.setAllUserDataFromJavascript(json)			}
			function setResultsMetaFromJavascript(json)			{ resultsJsInterface.setResultsMetaFromJavascript(json)			}
			function duplicateAnalysis(id)						{ resultsJsInterface.duplicateAnalysis(id)						}
			function showDependenciesInAnalysis(id, optName)	{ resultsJsInterface.showDependenciesInAnalysis(id, optName)	}
			function showRSyntaxInResults(show)					{ resultsJsInterface.showRSyntaxInResults(show)					}

			function showAnalysesMenu(options)
			{
				// FIXME: This is a mess
				// TODO:  1. remove redundant computations
				//        2. move everything to one place :P

				var optionsJSON  = JSON.parse(options);
				var functionCall = function (index)
				{
					var name		= customMenu.props['model'].getName(index);
					var jsfunction	= customMenu.props['model'].getJSFunction(index);

					customMenu.hide()

					if (name === 'hasExportResults')				{ fileMenuModel.exportResultsInteractive();		return; }
					if (name === 'hasRefreshAllAnalyses')			{ resultsJsInterface.refreshAllAnalyses();		return;	}
					if (name === 'hasRemoveAllAnalyses')			{ resultsJsInterface.removeAllAnalyses();		return; }
					if (name === 'hasCopy' || name === 'hasCite')	  resultsJsInterface.purgeClipboard();

					resultsJsInterface.runJavaScript(jsfunction);

					if (name === 'hasEditTitle' || name === 'hasNotes')
						resultsJsInterface.packageModified();

				}

				var selectedOptions = []
				for (var key in optionsJSON)
					if (optionsJSON.hasOwnProperty(key) && optionsJSON[key] === true)
						selectedOptions.push(key)

				resultMenuModel.setOptions(options, selectedOptions);

				var props = {
					"model"			: resultMenuModel,
					"functionCall"	: functionCall
				};

				customMenu.toggle(resultsView, props, (optionsJSON['rXright'] + 10) * preferencesModel.uiScale, optionsJSON['rY'] * preferencesModel.uiScale);

				customMenu.scrollOri		= resultsView.scrollPosition;
				customMenu.menuScroll.x		= Qt.binding(function() { return -1 * (resultsView.scrollPosition.x - customMenu.scrollOri.x) / resultsView.zoomFactor; });
				customMenu.menuScroll.y		= Qt.binding(function() { return -1 * (resultsView.scrollPosition.y - customMenu.scrollOri.y) / resultsView.zoomFactor; });
				customMenu.menuMinIsMin		= true
			}
		}
	}
}
