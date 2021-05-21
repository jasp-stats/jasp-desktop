//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtWebEngine
import QtWebChannel
import JASP.Widgets
import JASP.Controls
import QtQuick.Controls

Item
{
	id: splitViewContainer

	// The MainPage has 3 panels: Data, Analyses form and Results.
	// There are 3 configurations: only data, only analyses with results or all 3 panels.
	// Between these 3 panels, there are 2 handles: handleBetweenDataAndAnalyses & handleBetweenAnalysesAndResults: if there are only data
	// then no handle is displayed, if there are only analyses with results, then only handleBetweenAnalysesAndResults is displayed, and if
	// there are data and analyses, then both handles are displayed.
	// We cannot use the QML SplitView, since we cannot use click events on the handles (since Qt 6.3.2),
	// so to simulate the handles between the panels, we just use rectangles that can be dragged on the X axe.
	// The Analyses form panel has always the same width, so if we drag the first handle, the second handle should move at the same pace.
	// To coordinate the panels in function of the handles movement, all positions and widths of the panels are deduced from the position
	// of handleBetweenDataAndAnalyses (except when handleBetweenAnalysesAndResults is dragged, then this handle determine temporary the
	// position of the rest)

	property bool hasData:		mainWindow.dataAvailable
	property bool hasAnalysis:	mainWindow.analysesAvailable

	function minimizeDataPanel()
	{
		handleDataAnalyses.x = 0

	}

	function maximizeDataPanel()
	{
		handleDataAnalyses.x = splitViewContainer.width - (hasAnalysis ? handleAnalysesResults.width : 0)
	}

	Connections
	{
		target:		analysesModel
		function onAnalysisAdded()
		{
			// When adding an analysis, if the analyses pane is cut or the results pane has not enough space, hide the data panel
			if (resultsPane.width < jaspTheme.resultWidth || analysesPane.x < 0)
				minimizeDataPanel()
		}
	}

	Connections
	{
		target:		mainWindow
		function	onHideDataPanel() { minimizeDataPanel() }
	}

	onHasDataChanged:
	{
		if (hasData && !hasAnalysis)	maximizeDataPanel()
		else							minimizeDataPanel()
	}

	onWidthChanged:
	{
		if (handleDataAnalyses.visible && handleDataAnalyses.x > (width - handleDataAnalyses.width)) maximizeDataPanel()
	}

	DataPanel
	{
		id:						dataPanel
		visible:				hasData
		width:					hasAnalysis ? handleDataAnalyses.x + 1 : parent.width // -1 is for the border to the right
		height:					parent.height
	}

	JASPSplitHandle
	{
		id:					handleDataAnalyses
		x:					0 // All other items are depending on this point
		visible:			hasData && hasAnalysis
		onArrowClicked:
		{
			if (pointingLeft) minimizeDataPanel()
			else maximizeDataPanel()
		}
		pointingLeft:		x > 0
		toolTipArrow:		pointingLeft ? qsTr("Hide data")  : qsTr("Show data")
		toolTipDrag:		pointingLeft ? qsTr("Resize data/results") : qsTr("Drag to show data")
		onXChanged:			checkPosition(false)
		onDraggingChanged:	checkPosition(true)

		function checkPosition(forceCheck)
		{
			if (forceCheck || !dragging)
			{
				// When there is no data, the handle is not visible, but is still used to position the other items: then this handle might be negative.
				// Also when the analysesPane is open, then you may drag this handle outside at the left of the container.
				if (x < 0 && !analysesModel.visible && hasData)
					x = 0

				else if (x + width > parent.width)
					x = parent.width - width // Take care that this handle is always inside the container
			}
		}
	}

	Item
	{
		id:				analysesPane
		anchors.left:	handleDataAnalyses.right
		width:			analysesForm.width + extraBorder.width
		height:			parent.height

		Rectangle
		{
			// When there is an analysis without data, add an axtra border at the left side of the analysis form
			id:						extraBorder
			width:					visible ? jaspTheme.splitHandleWidth : 0
			visible:				!hasData && hasAnalysis && analysesModel.visible
			color:					jaspTheme.uiBackground
			border.width:			1
			border.color:			jaspTheme.uiBorder
			anchors
			{
				top:				parent.top
				bottom:				parent.bottom
				left:				parent.left
				leftMargin:			-1
				topMargin:			-1
				bottomMargin:		-1
			}

		}

		AnalysisForms
		{
			id:						analysesForm
			visible:				hasAnalysis && analysesModel.visible
			width:					hasAnalysis ? implicitWidth : 0
			height:					parent.height
			x:						extraBorder.width
		}
	}


	JASPSplitHandle
	{
		id:						handleAnalysesResults
		anchors.left:			!dragging ? analysesPane.right : undefined
		visible:				hasAnalysis && !ribbonModel.dataMode
		pointingLeft:			analysesModel.visible
		onArrowClicked:			analysesModel.visible = !analysesModel.visible
		toolTipDrag:			hasData	? (handleAnalysesResults.pointingLeft	? qsTr("Resize data/results")	: qsTr("Drag to show data")) : ""
		toolTipArrow:			analysesModel.visible							? qsTr("Hide input options")	: qsTr("Show input options")
		removeLeftBorder:		!analysesModel.visible

		Binding
		{
			// When dragging, the handleDataAnalyses must follow the movement of handleAnalysesResults
			target:			handleDataAnalyses
			property:		"x"
			value:			handleAnalysesResults.x + handleAnalysesResults.dragX - handleDataAnalyses.width - analysesPane.width
			when:			handleAnalysesResults.dragging
			restoreMode:	Binding.RestoreNone
		}

		onDraggingChanged:
		{
			if (dragging)
				return

			if (!hasData && !analysesModel.visible && x > 0)
				analysesModel.visible = true

			checkPosition(true)

		}

		onXChanged: checkPosition(false)

		function checkPosition(forceCheck)
		{
			if((!visible || dragging) && !forceCheck)
				return

			if (x < 0)
			{
				// We have moved the handle outside the container to the left: the analyses form panel must be hidden, and take care that the handleDataAnalyses appears.
				//analysesModel.visible = false
				handleDataAnalyses.x = - (handleDataAnalyses.width + analysesPane.width)
			}
			else if (!hasData && x > analysesPane.width)
				// If there is no data, the handle should not move more to the right than the widh of the analyses form panel.
				handleDataAnalyses.x = - handleDataAnalyses.width
		}

	}

	Rectangle
	{
		id:						resultsPane
		anchors
		{
			top:				parent.top
			left:				handleAnalysesResults.right
			right:				parent.right
			bottom:				parent.bottom
		}
		visible:				hasAnalysis && !ribbonModel.dataMode
		color:					analysesModel.currentAnalysisIndex !== -1 ? jaspTheme.uiBackground : jaspTheme.white

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
				case Qt.Key_PageDown:	resultsView.runJavaScript("windows.pageDown();");	break;
				case Qt.Key_PageUp:		resultsView.runJavaScript("windows.pageUp();");		break;
				}
			}

			onNavigationRequested: (request)=>
			{
				if(request.navigationType === WebEngineNavigationRequest.ReloadNavigation || request.url == resultsJsInterface.resultsPageUrl)
					request.accept()
				else
				{
					if(request.navigationType === WebEngineNavigationRequest.LinkClickedNavigation)
						Qt.openUrlExternally(request.url);
					request.reject();
				}
			}

			onLoadingChanged: (loadRequest)=>
			{
				resultsJsInterface.resultsLoaded = loadRequest.status === WebEngineView.LoadSucceededStatus;
				setTranslatedResultsString();
			}



			Connections
			{
				target:		resultsJsInterface
				function onRunJavaScriptSignal(js)			{ resultsView.runJavaScript(js); }
				function onScrollAtAllChanged(scrollAtAll)	{ resultsView.runJavaScript("window.setScrollAtAll("+(scrollAtAll ? "true" : "false")+")"); }

				function onExportToPDF(pdfPath)
				{
					if(preferencesModel.currentThemeName !== "lightTheme")
						resultsJsInterface.setThemeCss("lightTheme");

					resultsJsInterface.unselect(); //Otherwise we get the selected analysis highlighted in the pdf...
					resultsView.printToPdf(pdfPath);
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

			function setTranslatedResultsString()
			{
				if(resultsJsInterface.resultsLoaded)
					runJavaScript("window.setAnalysesTitle(\"" + resultsString + "\");");
			}

			QtObject
			{
				id:				resultsJsInterfaceInterface
				WebChannel.id:	"jasp"

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
}
