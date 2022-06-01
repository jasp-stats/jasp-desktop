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

	onWidthChanged:
	{
		if(!mainWindow.analysesAvailable)												data.maximizeData();
		else if(data.wasMaximized)														return; //wasMaximized binds!
		else if(splitViewContainer.width <= data.width + jaspTheme.splitHandleWidth)	data.maximizeData();
	}

	onVisibleChanged:	if(visible && !mainWindow.dataPanelVisible)	data.minimizeData();

	SplitView
	{
		id:				panelSplit
		orientation:	Qt.Horizontal
		height:			parent.height
		width:			parent.width + hackySplitHandlerHideWidth + leftHandSplitHandlerSpace
		x:				-leftHandSplitHandlerSpace

		//hackySplitHandlerHideWidth is there to create some extra space on the right side for the analysisforms I put inside the splithandle. https://github.com/jasp-stats/INTERNAL-jasp/issues/144
		//And also on the left side to allow it to move out of the screen there.
		property int	hackySplitHandlerHideWidth:	( mainWindow.analysesAvailable ? (analysesModel.visible ? leftHandSplitHandlerSpace : 0 ) + jaspTheme.splitHandleWidth : 0 )
		property int	leftHandSplitHandlerSpace:	jaspTheme.formWidth + 3 + jaspTheme.scrollbarBoxWidthBig

		onResizingChanged: if(!resizing) data.makeSureHandleVisible();

		DataPanel
		{
			id:						data
			visible:				mainWindow.dataAvailable || fakeEmptyDataForSumStatsEtc //|| analysesModel.count > 0
			z:						1
			leftHandSpace:			panelSplit.leftHandSplitHandlerSpace

			property real baseMaxWidth:					fakeEmptyDataForSumStatsEtc ? 0 : splitViewContainer.width - (mainWindow.analysesAvailable ? jaspTheme.splitHandleWidth : 0)
			property real maxWidth:						leftHandSpace + baseMaxWidth
			property bool fakeEmptyDataForSumStatsEtc:	!mainWindow.dataAvailable && mainWindow.analysesAvailable
			property bool wasMaximized:					false

			function makeSureHandleVisible()
			{
				if(!analysesModel.visible && data.width < leftHandSpace)
					data.minimizeData();
				else if(data.width - leftHandSpace > splitViewContainer.width - jaspTheme.splitHandleWidth)
					data.maximizeData();
			}

			onWidthChanged:
			{

				var iAmBig = width > leftHandSpace;
				if(iAmBig != mainWindow.dataPanelVisible)
					mainWindow.dataPanelVisible = iAmBig

				if(fakeEmptyDataForSumStatsEtc)
				{
					mainWindow.dataPanelVisible = false;
					width = leftHandSpace;
				}

				if(data.width != data.maxWidth)
					data.wasMaximized = false;

				makeSureHandleVisible();
			}

			function maximizeData()	{ SplitView.preferredWidth = Qt.binding(function() { return data.maxWidth; });	data.wasMaximized = true;  }
			function minimizeData()	{ SplitView.preferredWidth = Qt.binding(function() { return leftHandSpace; });	data.wasMaximized = false; }

			Connections
			{
				target:		mainWindow
				function onDataPanelVisibleChanged(visible)
				{
					if (visible && data.width		<=	data.leftHandSpace)	data.maximizeData();
					else if(!visible)										data.minimizeData();
				}
			}

			Connections
			{
				target:	analysesModel
				function onVisibleChanged(visible)
				{
					if(!visible)
						data.makeSureHandleVisible();
				}
			}
		}


		handle: Item
		{
			implicitWidth:			splitHandle.width + analyses.implicitWidth
			width:					implicitWidth



			JASPSplitHandle
			{
				id:				splitHandle
				onArrowClicked:	mainWindow.dataPanelVisible = !mainWindow.dataPanelVisible
				pointingLeft:	mainWindow.dataPanelVisible
				showArrow:		mainWindow.dataAvailable
				toolTipArrow:	mainWindow.dataAvailable	? (mainWindow.dataPanelVisible ? qsTr("Hide data")  : qsTr("Show data")) : ""
				toolTipDrag:	mainWindow.dataPanelVisible ? qsTr("Resize data/results") : qsTr("Drag to show data")
				dragEnabled:	mainWindow.dataAvailable && mainWindow.analysesAvailable
			}

			AnalysisForms
			{
				id:						analyses
				z:						-1
				visible:				mainWindow.analysesAvailable
				width:					visible ? implicitWidth : 0
				anchors.top:			parent.top
				anchors.bottom:			parent.bottom
				anchors.left:			splitHandle.right
			}
		}

		Rectangle
		{
			id:								giveResultsSomeSpace
			SplitView.preferredWidth:		jaspTheme.resultWidth + panelSplit.hackySplitHandlerHideWidth
			SplitView.fillWidth:			true
			z:								3
			visible:						mainWindow.analysesAvailable
			onVisibleChanged:				if(visible) width = jaspTheme.resultWidth; else data.maximizeData()
			color:							analysesModel.currentAnalysisIndex !== -1 ? jaspTheme.uiBackground : jaspTheme.white

			Connections
			{
				target:				analysesModel
				function			onAnalysisAdded()
				{
					//make sure we get to see the results!

					var inputOutputWidth	= splitViewContainer.width - (data.width + jaspTheme.splitHandleWidth)
					var remainingDataWidth	= Math.max(0, data.width - (jaspTheme.splitHandleWidth + jaspTheme.resultWidth));

					if(inputOutputWidth < 100 * preferencesModel.uiScale)
						 mainWindow.dataPanelVisible = false;
					else if(inputOutputWidth < jaspTheme.resultWidth)
					{
						if(remainingDataWidth === 0)	mainWindow.dataPanelVisible = false;
						else							data.width = remainingDataWidth
					}
				}
			}

			WebEngineView
			{
				id:						resultsView

				anchors
				{
					top:				parent.top
					left:				parent.left
					bottom:				parent.bottom
				}

				width:					giveResultsSomeSpace.width - panelSplit.hackySplitHandlerHideWidth

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
}
