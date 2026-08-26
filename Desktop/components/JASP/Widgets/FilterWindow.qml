import QtQuick
import QtQuick.Controls
import JASP.Controls	as JaspControls
import "FilterConstructor"
import JASP

import QtQuick.Layouts

FocusScope
{
	id:							filterContainer

	function close()
	{
		filterModel.filterVisible = false
	}


	function applyAndSendFilter(newFilter)
	{
		filterModel.applyRFilter(newFilter) //Triggers send in FilterModel
	}

	function resetFilter()
	{
		filterModel.resetRFilter()
	}
	signal rCodeChanged(string rScript)
	
	function askIfChanged(closeFunc)
	{
		if(filterModel.showEasyFilter)
			easyFilterConstructor.askIfChanged(closeFunc)	
		else
			filterEditRectangle.askIfChanged(closeFunc) 
	}

	Rectangle
	{
		anchors.fill:	parent
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		z:				-100
	}
	
	Rectangle
	{
		id:				backgroundFiltersTabs
		color:			jaspTheme.uiBackground
		border.width:	1
		border.color:	jaspTheme.buttonBorderColor
		z:				1
		height:			filtersTabs.height + jaspTheme.generalAnchorMargin + 1 //+1 for line on the bottom
		anchors
		{
			top:		parent.top
			left:		parent.left
			right:		parent.right
		}
		clip:			true
		
		Flickable
		{
			anchors
			{
				top:			backgroundFiltersTabs.top
				left:			backgroundFiltersTabs.left
				right:			backgroundFiltersTabs.right
				margins:		jaspTheme.generalAnchorMargin
			}
			
			implicitHeight:		filtersTabs.height
			height:				filtersTabs.height
			
			contentHeight:		filtersTabs.height
			contentWidth:		filtersTabs.width
		
			RowLayout
			{
				id:		filtersTabs
				z:		2
				anchors
				{
					top:			backgroundFiltersTabs.top
					left:			backgroundFiltersTabs.left
					right:			backgroundFiltersTabs.right
					margins:		jaspTheme.generalAnchorMargin
				}
				
				Repeater
				{
					model:	filterModel.filterDropDownList
					
					FilterWindowTabButton
					{
						property string labelText:	modelData["label"]
						property string valueText:	modelData["value"]
						
						text:			doSeparator ? "" : labelText
						
						onClicked:		
						{
							if(!doSeparator)
							{
								filterContainer.askIfChanged(function() { filterModel.currentFilterId = valueText } )
							}
							else
							{
								filterContainer.askIfChanged(function() {  filterModel.addFilter(labelText) } )
							}
						}
						
						buttonActive:	!hideButtoness && filterModel.currentFilterId  == valueText
						showTextField:	buttonActive && filterModel.currentFilter != "DEFAULT_FILTER"
						doSeparator:	valueText == "---"
						hideButtoness:	valueText == "-" || valueText == "*"
						hideButtonCol:	valueText == "*" ? jaspTheme.textEnabled : jaspTheme.textDisabled
						
						theButton.color:		doSeparator ||  hideButtoness ? jaspTheme.uiBackground		: !buttonActive ? theButton.defaultColor : jaspTheme.white
						theButton.border.width:	doSeparator ||  hideButtoness ? 0							: 1
						theButton.border.color:	doSeparator ||  hideButtoness ? jaspTheme.buttonBorderColor	: theButton.defaultBorderColor
						
						//Component.onCompleted: {
						//	messages.log("FilterWindowTabButton")
						//	messages.log( JSON.stringify(modelData ))	
						//}
						
					}
				}
				
				//FilterWindowTabButton
				//{
				//	id:					addFilterButtonEasy
				//	iconSource:			jaspTheme.iconPath + "/round_addition.png"
				//	onClicked:			
				//	theButton.width:	theButton.height
				//}
				
				Item
				{
					Layout.fillWidth: true	
				}
			}
		}
	}
	
	Item
	{
		id:				minWidthCollector

		property int minWidth: 500 * preferencesModel.uiScale

		anchors
		{
			top:			backgroundFiltersTabs.bottom
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			rightMargin:	Math.min(0, filterContainer.width - minWidthCollector.minWidth)
		}

		Item
		{
			anchors.fill:		parent
			anchors.margins:	1
			anchors.topMargin:	0
			visible:			filterModel.showEasyFilter


			ScriptConstructor
			{
				id:					easyFilterConstructor
				mode:				ScriptConstructor.Filter
				columnsModel:		columnsModel
				constructorJson:	filterModel.filter.constructorJson
				clip:				true

				anchors
				{
					bottom:	applyEasyFilter.top
					right:	parent.right
					left:	parent.left
					top:	parent.top
				}

				onApplyRequested: function(json, rCode)
				{
					filterModel.applyConstructorJson(json)
					filterModel.filter.constructorR = rCode
				}

				onRCodeChanged: filterContainer.rCodeChanged(rScript)

				function askIfChanged(closeFunc)
				{
					if(jsonChanged() || !lastCheckPassed)
					{
						easySaveDialog.closeFunc = closeFunc
						easySaveDialog.open()
					}
					else
						closeFunc()
				}

				SaveDiscardCancelDialog
				{
					id:		easySaveDialog

					title:	qsTr("Would you like to save your changes to the Filter?")
					text:	qsTr("Your changes will be lost if you don't save them.")

					property var closeFunc: undefined

					onSave:		if(easyFilterConstructor.checkAndApply()) closeFunc();
					onDiscard:	{ easyFilterConstructor.initializeFromJSON(); closeFunc(); }
				}
			}

			JaspControls.HelpButton
			{
				id:					helpEasyRectangularButton
				height:				33 * jaspTheme.uiScale
				width:				height
				radius:				height
				buttonPadding:		6 * preferencesModel.uiScale
				helpMD:				allHelp.easyfilterconstructor;
				toolTip:			qsTr("Open Documentation")
				anchors
				{
					right:			closeButton.left
					top:			parent.top
				}
			}
			JaspControls.MenuButton
			{
				id:					closeButton
				height:				33 * jaspTheme.uiScale
				width:				height
				radius:				height
				iconSource:			jaspTheme.iconPath + "collapse.png"
				onClicked:			filterContainer.askIfChanged(function() { filterWindow.close() } )
				toolTip:			qsTr("Close filter window")
				anchors
				{
					top:			parent.top
					right:			parent.right
					rightMargin:	jaspTheme.generalAnchorMargin
				}
			}

			JaspControls.RectangularButton
			{
				id:			rRectangularButton
				iconSource: jaspTheme.iconPath + "/R.png"
				onClicked:	filterContainer.askIfChanged(function() { filterModel.showEasyFilter = false } )
				width:		height
				toolTip:	qsTr("Switch to the R filter")
				anchors
				{
					left:	parent.left
					bottom:	parent.bottom
				}
			}

			JaspControls.RectangularButton
			{
				id:			showInactiveFilteredButtonEasy
				iconSource: dataSetModel.showInactive ? jaspTheme.iconPath + "/eyeOpen.png" : jaspTheme.iconPath + "/eyeClosed.png"
				onClicked:	dataSetModel.showInactive = !dataSetModel.showInactive
				width:		height
				toolTip:	(dataSetModel.showInactive ? qsTr("Hide rows that were filtered out.") : qsTr("Show rows that were filtered out."))
				anchors
				{
					left:	rRectangularButton.right
					bottom:	parent.bottom
					top:	rRectangularButton.top
				}
			}
			

			JaspControls.RectangularButton
			{
				property bool showApplyNotApplied: easyFilterConstructor.somethingChanged

				id:				applyEasyFilter
				text:			showApplyNotApplied ? qsTr("Apply pass-through filter") : qsTr("Filter applied")
				enabled:		easyFilterConstructor.somethingChanged
				onClicked:		easyFilterConstructor.checkAndApply()
				toolTip:		showApplyNotApplied ? qsTr("Click to apply filter") : qsTr("Filter is already applied")
				anchors
				{
					left:	showInactiveFilteredButtonEasy.right
					right:	parent.right
					bottom: parent.bottom
					top:	rRectangularButton.top
				}

			}
		}

		Item
		{
							id:						rFilterFields
							visible:				!filterModel.showEasyFilter
							anchors.fill:			parent
							anchors.margins:		1
							anchors.topMargin:		-1
			property real	desiredMinimumHeight:	filterButtons.height + (filterErrorScroll.visible ? filterErrorScroll.height : 0 ) + filterEditRectangle.desiredMinimumHeight

			Rectangle
			{
								id:						filterEditRectangle
								color:					jaspTheme.white
								border.width:			1
								border.color:			jaspTheme.uiBorder
				property real	desiredMinimumHeight:	applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight

				anchors
				{
					top:		parent.top
					bottom:		filterErrorScroll.top
					left:		parent.left
					right:		parent.right
				}

				Image
				{
					id:							backgroundImage

					source:						jaspTheme.iconPath + "/filterConstructorBackground.png"
					anchors.centerIn:			parent

					property real widthScale:	parent.width  / implicitWidth
					property real heightScale:	parent.height / implicitHeight
					property real ratio:		Math.min(Math.min(widthScale, heightScale), 1.0) * 0.5

					width:						implicitWidth * ratio
					height:						implicitHeight * ratio
				}

				ScrollView
				{
					id:				filterScroller
					anchors.fill:	parent
					clip:			true
					contentWidth:	width
					contentHeight:	filterGeneratedBox.height + filterEditBox.height

					Rectangle
					{
						id:				filterGeneratedBox
						height:			filterGeneratedEdit.height
						color:			"transparent"
						border.color:	jaspTheme.uiBorder
						border.width:	1

						anchors
						{
							top:	parent.top
							left:	parent.left
							right:	parent.right
						}


						TextArea
						{
							JaspControls.RSyntaxHighlighterQuick
							{
								textDocument:		filterGeneratedEdit.textDocument
							}
							
							id:						filterGeneratedEdit
							anchors.top:			filterGeneratedBox.top
							anchors.left:			resetAllGeneratedFilters.right
							anchors.right:			filterGeneratedBox.right
							text:					filterModel.filter.generatedFilter
							height:					implicitHeight
							readOnly:				true
							color:					jaspTheme.grayDarker
							selectByMouse:			true
							onActiveFocusChanged:	if(!activeFocus) deselect()
							font.pixelSize:         baseFontSize * preferencesModel.uiScale
							font.family:			jaspTheme.fontCode.family
							wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere

						}

						JaspControls.RectangularButton
						{
							id:						resetAllGeneratedFilters
							width:					(workspace.shownDataSet && workspace.shownDataSet.columnsLabelFilteredCount > 0) ? height : 0
							height:					filterGeneratedBox.height
							iconSource:				jaspTheme.iconPath + "eraser_all.png"
							visible:				workspace.shownDataSet && workspace.shownDataSet.columnsLabelFilteredCount > 0
							toolTip:				qsTr("Reset all checkmarks on all labels")
							onClicked:				dataSetModel.resetAllFilters()

							anchors.left:			parent.left
							anchors.verticalCenter:	parent.verticalCenter
							anchors.margins:		1

						}
					}

					Item
					{
						//Must be here because otherwise filterEdit turns its clipping on, because it is in a scrollview...
						id:				filterEditBox
						height:			filterEdit.height
						anchors.top:	filterGeneratedBox.bottom
						anchors.left:	parent.left
						anchors.right:	parent.right

						TextArea
						{

							JaspControls.RSyntaxHighlighterQuick
							{
								textDocument:		filterEdit.textDocument
							}
							
							id:						filterEdit
							height:					contentHeight + 30
							selectByMouse:			true
							onActiveFocusChanged:	if(!activeFocus) deselect()
							font.family:			jaspTheme.fontCode.family
							font.pixelSize:         baseFontSize * preferencesModel.uiScale
							wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere
							color:					jaspTheme.textEnabled
							text:					filterModel.filter.rFilter

							property bool changedSinceLastApply: text !== filterModel.filter.rFilter

							Keys.onReturnPressed:	(keyEvent) => {
														if(keyEvent.modifiers & Qt.ControlModifier)
														{
															if(filterEdit.changedSinceLastApply)
																filterWindow.applyAndSendFilter(filterEdit.text)
														}
														else
															keyEvent.accepted = false
													}

							anchors
							{
								top:	parent.top
								left:	parent.left
								right:	parent.right
							}
						}
					}
				}

				function askIfChanged(closeFunc)
				{
					if(filterEdit.changedSinceLastApply)
					{
						saveDialog.closeFunc = closeFunc
						saveDialog.open()
					}
					else
						closeFunc()
				}

				SaveDiscardCancelDialog
				{
					id:		saveDialog

					title:	qsTr("Filter Changed")
					text:	qsTr("There are unapplied changes to your filter; what would you like to do?")

					property var closeFunc: undefined

					onSave:		{ filterWindow.applyAndSendFilter(filterEdit.text); closeFunc(); }
					onDiscard:	
					{
						
						filterEdit.text = filterModel.filter.rFilter
						closeFunc();
					}
				}
			}

			ScrollView
			{
				id:				filterErrorScroll
				height:			filterWindow.minimumHeightTextBoxes

				anchors
				{
					left:	parent.left
					right:	parent.right
					bottom: filterButtons.top
				}

				TextArea
				{
					id:						filterError
					color:					jaspTheme.red
					readOnly:				true
					text:					filterModel.filter.filterErrorMsg + "\n"
					selectByMouse:			true
					onActiveFocusChanged:	if(!activeFocus) deselect()
					font.family:			jaspTheme.fontCode.family
					font.pixelSize:			baseFontSize * preferencesModel.uiScale

					states:
					[
						State
						{
							name: "closed"
							when: filterModel.filter.filterErrorMsg.length === 0
							PropertyChanges { target: filterErrorScroll; visible: false; height: 0 }
						},
						State
						{
							name: "opened"
							when: filterModel.filter.filterErrorMsg.length > 0
							PropertyChanges { target: filterErrorScroll; visible: true; height: filterError.contentHeight} //Math.min( , filterWindow.minimumHeightTextBoxes)
						}
					]
				}
			}

			Item
			{
				id:				filterButtons
				height:			closeRectangularButton.height
				anchors
				{
					left:		parent.left
					right:		parent.right
					bottom:		parent.bottom
				}

				JaspControls.RectangularButton
				{
					id:				easyRectangularButton
					iconSource:		jaspTheme.iconPath + "/NotR.png"
					onClicked:		filterEditRectangle.askIfChanged(function (){ filterModel.showEasyFilter = true })
					width:			visible ? height : 0
					toolTip:		qsTr("Switch to the drag and drop filter")
					anchors
					{
						left:		parent.left
						bottom:		parent.bottom
						top:		closeRectangularButton.top
					}
				}

				JaspControls.RectangularButton
				{
					id:			showInactiveFilteredButtonR
					iconSource: dataSetModel.showInactive ? jaspTheme.iconPath + "/eyeOpen.png" : jaspTheme.iconPath + "/eyeClosed.png"
					onClicked:	dataSetModel.showInactive = !dataSetModel.showInactive
					width:		height
					toolTip:	(!dataSetModel.showInactive ? qsTr("Hide rows that were filtered out.") : qsTr("Show rows that were filtered out."))
					anchors
					{
						left:	easyRectangularButton.right
						bottom:	parent.bottom
						top:	easyRectangularButton.top
					}
				}
				

				JaspControls.RectangularButton
				{
					id:			clearRectangularButton
					iconSource: jaspTheme.iconPath + "eraser.png"
					onClicked:	filterWindow.resetFilter()
					width:		visible ? implicitWidth : 0
					height:		filterContainer.buttonsHeight
					visible:	filterEdit.text !== filterModel.filter.defaultRFilter
					toolTip:	qsTr("Reset to default filter")

					anchors
					{
						left:	showInactiveFilteredButtonR.right
						bottom: parent.bottom
						top:	closeRectangularButton.top
					}
				}

				JaspControls.RectangularButton
				{
					id: applyFilter

					property bool filterIsDefault: filterEdit.text === filterModel.filter.defaultRFilter

					text:			qsTr("Apply pass-through filter")
					anchors.left:	clearRectangularButton.right
					anchors.right:	helpButton.left
					anchors.bottom:	parent.bottom
					anchors.top:	closeRectangularButton.top

					onClicked:		{ forceActiveFocus(); filterWindow.applyAndSendFilter(filterEdit.text) }

					toolTip:		qsTr("Click to apply filter")
				}

				JaspControls.HelpButton
				{
					id:				helpButton
					height:			33 * jaspTheme.uiScale
					width:			height
					buttonPadding:	6 * preferencesModel.uiScale

					anchors.right:	closeRectangularButton.left
					anchors.bottom: parent.bottom
					anchors.top:	closeRectangularButton.top
					helpMD:			allHelp.rfilterconstructor
					toolTip:		qsTr("Open Documentation")
				}


				JaspControls.RectangularButton
				{
					id:				closeRectangularButton
					iconSource:		jaspTheme.iconPath + "cross.png"
					anchors.right:	parent.right
					anchors.bottom: parent.bottom

					onClicked:		filterEditRectangle.askIfChanged(function (){ filterWindow.close() })
					toolTip:		qsTr("Hide filter")
				}
			}
		}
	}
}
