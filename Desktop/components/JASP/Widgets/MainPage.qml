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

import QtQuick			2.12
import QtWebEngine		1.7
import QtWebChannel		1.0
import JASP.Widgets		1.0
import JASP.Controls	1.0
import QtQuick.Controls 6.0

Item
{
	id: splitViewContainer

	property bool hasData:		mainWindow.dataAvailable
	property bool hasAnalysis:	mainWindow.analysesAvailable

	Connections
	{
		target:		mainWindow
		function onDataPanelVisibleChanged(visible)
		{
			if (visible) data.maximizeData()
			else data.minimizeData()
//				if (visible && data.width		<=	data.leftHandSpace)	data.maximizeData();
//				else if(!visible)										data.minimizeData();
		}
	}

/*	Connections
	{
		target:	analysesModel
		function onVisibleChanged(visible)
		{
			if(!visible) makeSureHandleVisible();
		}
	} */

	/*onWidthChanged:
	{
		if(!hasData)												data.maximizeData();
		else if(data.wasMaximized)									return; //wasMaximized binds!
		else if(width <= data.width + jaspTheme.splitHandleWidth)	data.maximizeData();
	}*/

	//onVisibleChanged:	if(visible && !mainWindow.dataPanelVisible)	data.minimizeData();


//		onResizingChanged: if(!resizing) data.makeSureHandleVisible();

	DataPanel
	{
		id:						data
		visible:				hasData
//		leftHandSpace:			panelSplit.leftHandSplitHandlerSpace
		width:					splitHandle1.x
		height:					parent.height

/*		property real baseMaxWidth:					fakeEmptyDataForSumStatsEtc ? 0 : splitViewContainer.width - (mainWindow.analysesAvailable ? jaspTheme.splitHandleWidth : 0)
		property real maxWidth:						leftHandSpace + baseMaxWidth
		property bool fakeEmptyDataForSumStatsEtc:	!mainWindow.dataAvailable && mainWindow.analysesAvailable
		property bool wasMaximized:					false


		onWidthChanged:
		{

			var iAmBig = width > leftHandSpace;
			if(iAmBig != mainWindow.dataPanelVisible)
				mainWindow.dataPanelVisible = iAmBig

			if(fakeEmptyDataForSumStatsEtc)
			{
				mainWindow.dataPanelVisible = false;
				minimizeData();
			}

			if(data.width != data.maxWidth)
				data.wasMaximized = false;

			makeSureHandleVisible();
		}
		*/

		function maximizeData()				{ splitHandle1.x = parent.width - (hasAnalysis ? splitHandle1.width : 0) }
		function minimizeData()				{ splitHandle1.x = 0; }
	}

	JASPSplitHandle
	{
		id:				splitHandle1
		x:				100 //splitHandle2.x - width - analyses.width
		visible:		hasData && hasAnalysis
		onArrowClicked:	mainWindow.dataPanelVisible = !mainWindow.dataPanelVisible
		pointingLeft:	mainWindow.dataPanelVisible
		showArrow:		true
		toolTipArrow:	mainWindow.dataPanelVisible ? qsTr("Hide data")  : qsTr("Show data")
		toolTipDrag:	mainWindow.dataPanelVisible ? qsTr("Resize data/results") : qsTr("Drag to show data")
		dragEnabled:	true
		onXChanged:		checkPosition()
		onDraggingChanged: checkPosition()

		function checkPosition()
		{
			if (!dragging && visible)
			{
				if (x < 0 && !analysesModel.visible)	x = 0
				else if (x + width > parent.width)		x = parent.width - width
			}
		}
	}

	AnalysisForms
	{
		id:						analyses
		visible:				analysesModel.visible
		width:					analysesModel.visible ? implicitWidth : 0
		height:					parent.height
		x:						hasData ? (splitHandle1.x + splitHandle1.width) : 0
	}


	JASPSplitHandle
	{
		id:						splitHandle2
		x:						analyses.x + analyses.width
		visible:				hasAnalysis
		showArrow:				true
		pointingLeft:			analysesModel.visible
		onArrowClicked:			analysesModel.visible = !analysesModel.visible
		toolTipDrag:			hasData	? (mainWindow.dataPanelVisible ? qsTr("Resize data/results")  : qsTr("Drag to show data")) : ""
		toolTipArrow:			analysesModel.visible		? qsTr("Hide input options") : qsTr("Show input options")
		dragEnabled:			true
		onHandleDragging:		function(active, mouseArea)
		{
			if (active)
			{
				x = x // Remove binding to prevent binding loop
				splitHandle1.x = Qt.binding(function() { return splitHandle2.x + mouseArea.x - splitHandle1.width - analyses.width })
			}
			else
			{
				// Remove binding for splitHandle1 and restore binding for splitHandler2
				splitHandle1.x = splitHandle1.x
				x = Qt.binding(function() { return analyses.x + analyses.width})
				checkPosition()
			}
		}
		onXChanged: checkPosition()

		function checkPosition()
		{
			if (!dragging && visible)
			{
				if (x < 0)
				{
					if (hasData)	splitHandle1.x = - (splitHandle1.width + analyses.width)
					else			splitHandle1.x = 0
				}
			}
		}
	}

	Rectangle
	{
		id:								giveResultsSomeSpace
		x:								splitHandle2.x + splitHandle2.width
		width:							parent.width - x
		height:							parent.height

//		z:								3
		visible:						hasAnalysis
//		onVisibleChanged:				if(visible) width = jaspTheme.resultWidth; else data.maximizeData()
		color:							analysesModel.currentAnalysisIndex !== -1 ? jaspTheme.uiBackground : jaspTheme.white

		Connections
		{
			target:				analysesModel
			function			onAnalysisAdded()
			{
				data.minimizeData()
				//make sure we get to see the analyses + results!

/*				var desiredFormResWidth	= panelSplit.leftHandSplitHandlerSpace + jaspTheme.resultWidth
				var inputOutputWidth	= splitViewContainer.width - data.width + panelSplit.leftHandSplitHandlerSpace
				var remainingDataWidth	= Math.max(0, splitViewContainer.width - (desiredFormResWidth));

				//If data.width < leftHandSplit then it isnt in view anymore so we can just minimize it
				if(data.width < panelSplit.leftHandSplitHandlerSpace)
					data.minimizeData()
				//If the remaining width of the jasp window is smaller than resultwidth + formwidth just minimize data as well
				else if(inputOutputWidth < desiredFormResWidth)
					 mainWindow.dataPanelVisible = false;
				else if(mainWindow.dataPanelVisible)
				{
					//There was some space remaining but it aint much so hide datapanel
					if(remainingDataWidth < jaspTheme.formWidth / 2)
						mainWindow.dataPanelVisible = false;
					else
					{
						//otherwise check whether we can just keep showing everything at once?

						remainingDataWidth += panelSplit.leftHandSplitHandlerSpace;
						if(remainingDataWidth < data.width) //data can stay visible but should be smaller
							data.width = remainingDataWidth
						//else, keep as is
					}
				}
				//else keep as is
				*/
			}
		}

		WebEngineView
		{
			id:						resultsView
			clip:                   true

			anchors.fill:			parent
/*			anchors
			{
				top:				parent.top
				bottom:				parent.bottom
			}
*/
//			x:						1 + (Math.floor(parent.x) - parent.x)
//			width:					Math.floor(giveResultsSomeSpace.width - panelSplit.hackySplitHandlerHideWidth)

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
