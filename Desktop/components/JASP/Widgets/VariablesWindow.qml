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
import JASP
import JASP.Widgets
import JASP.Controls
import QtQuick.Controls as QTC
import QtQuick.Layouts
import "."
import "./FileMenu"


FocusScope
{
	id:			variablesContainer
	visible:	columnModel.visible

	property real calculatedBaseHeight:			common.height + jaspTheme.generalAnchorMargin
	property real calculatedMinimumHeight:		calculatedBaseHeight + (tabView.visible ?  0.28 * parent.height : 0)
	property real calculatedPreferredHeight:	calculatedBaseHeight + (tabView.visible ?  0.32 * parent.height : 0)
	property real calculatedMaximumHeight:		!tabView.visible ? calculatedBaseHeight :  parent.height * 0.7

	Connections
	{
		target: columnModel

		function onBeforeChangingColumn(chosenColumn)
		{
			if (columnModel.visible && columnModel.chosenColumn >= 0)
			{
				columnModel.columnName			= columnNameVariablesWindow.value
				columnModel.columnTitle			= columnTitleVariablesWindow.value
				columnModel.columnDescription	= columnDescriptionVariablesWindow.text
				columnModel.computedType		= computedTypeVariableWindow.value
				columnModel.currentColumnType	= columnTypeVariableWindow.value
			}
		}
		
		function onChosenColumnChanged(chosenColumn)
		{
			if(columnModel.chosenColumn > -1 && columnModel.chosenColumn < dataSetModel.columnCount())
				//to prevent the editText in the labelcolumn to get stuck and overwrite the next columns data... We have to remove activeFocus from it
				common.focus = true //So we just put it somewhere
		}
	}

	Item
	{
		id:		minWidthVariables

		property int minWidth: 600 * preferencesModel.uiScale

		anchors
		{
			fill:			parent
			rightMargin:	Math.min(0, variablesContainer.width - minWidth)
		}

		Rectangle
		{
			color:				jaspTheme.uiBackground
			border.color:		jaspTheme.uiBorder
			border.width:		1
			anchors.fill:		parent
			z:					-1
		}

		Item
		{
			id:					common
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.leftMargin: 0
			anchors.margins:	jaspTheme.generalAnchorMargin
			width:			    parent.width
			height:				Math.max(leftColumn.childrenRect.height, rightColumn.childrenRect.height) + 2 * jaspTheme.generalAnchorMargin

			Column
			{
				id:			leftColumn
				width:		Math.max(columnTypeVariableWindow.implicitWidth, computedTypeVariableWindow.implicitWidth, columnNameVariablesWindow.implicitWidth)
				spacing:	jaspTheme.rowGroupSpacing

				anchors
				{
					top:		common.top
					left:		common.left
					bottom:		common.bottom
					margins:	jaspTheme.generalAnchorMargin
				}

				property int labelWidth:	Math.max(columnTypeVariableWindow.controlLabel.implicitWidth, computedTypeVariableWindow.controlLabel.implicitWidth, columnNameVariablesWindow.controlLabel.implicitWidth)

				RowLayout
				{
					height:				longNameRow.height

					TextField
					{
						id:					columnNameVariablesWindow
						placeholderText:	qsTr("<Fill in the name of the column>")
						value:				columnModel.columnName
						onValueChanged:		if(columnModel.columnName !== value) columnModel.columnName = value
						undoModel:			columnModel
						editable:           columnModel.nameEditable
						label:				qsTr("Name: ")
						controlLabel.width:	leftColumn.labelWidth

					}
				}


				DropDown
				{
					id: columnTypeVariableWindow

					label:				qsTr("Column type: ")
					isBound:			false
					showVariableTypeIcon: true
					values:				columnModel.columnTypeValues
					currentValue:		columnModel.currentColumnType
					onValueChanged:		columnModel.currentColumnType = currentValue
					controlMinWidth:	200 * jaspTheme.uiScale
					controlLabel.width:	leftColumn.labelWidth
				}

				DropDown
				{
					id: computedTypeVariableWindow

					label:				qsTr("Computed type: ")
					values:				columnModel.computedTypeValues
					currentValue:		columnModel.computedType
					onValueChanged:		columnModel.computedType = currentValue
					visible:			columnModel.computedTypeEditable
					controlMinWidth:	200 * jaspTheme.uiScale

					controlLabel.width:	leftColumn.labelWidth
				}

				Item
				{
					//Layout.fillWidth:		true
					implicitWidth:			parent.width
					implicitHeight:			showAnalysisButton.height
					visible:				!columnModel.computedTypeEditable

					RoundedButton
					{
						id:					showAnalysisButton
						text:				qsTr("Show parent analysis")
						width:				parent.width - x
						x:					leftColumn.labelWidth
						onClicked:			computedColumnsInterface.showAnalysisFormForColumn(columnModel.columnName)
					}
				}
			}

			Column
			{
				id:			rightColumn
				spacing:	jaspTheme.rowGroupSpacing

				anchors
				{
					top:		common.top
					left:		leftColumn.right
					right:		common.right
					bottom:		common.bottom
					margins:	jaspTheme.generalAnchorMargin
				}

				RowLayout
				{
					id:					longNameRow
					width:				parent.width


					TextField
					{
						id:					columnTitleVariablesWindow
						label:				qsTr("Long name: ");
						placeholderText:	qsTr("<Fill in a more descriptive name of the column>")
						fieldWidth:			columnDescriptionVariablesWindow.width - closeButton.width
						value:				columnModel.columnTitle
						onValueChanged:		if(columnModel.columnTitle !== value) columnModel.columnTitle = value
						undoModel:			columnModel
						controlLabel.width:	Math.max(columnTitleVariablesWindow.controlLabel.implicitWidth, descriptionLabel.implicitWidth)

					}

					MenuButton
					{
						id:					closeButton
						height:				33 * jaspTheme.uiScale
						width:				height
						iconSource:			jaspTheme.iconPath + "close-button.png"
						onClicked:			{ computedColumnWindow.askIfChangedOrClose(); columnModel.visible = false }
						toolTip:			qsTr("Close variable window")
						radius:				height
					}
				}

				RowLayout
				{
					id:					descriptionRow
					width:				parent.width

					Label
					{
						id:		descriptionLabel
						text:	qsTr("Description: ")
						width:	Math.max(columnTitleVariablesWindow.controlLabel.implicitWidth, descriptionLabel.implicitWidth)

					}

					TextArea
					{
						id:					columnDescriptionVariablesWindow

						height:				columnTypeVariableWindow.height + computedTypeVariableWindow.height + rightColumn.spacing
						control.padding:	3 * jaspTheme.uiScale

						text:				columnModel.columnDescription
						onEditingFinished: 	if(columnModel.columnDescription !== text) columnModel.columnDescription = text
						applyScriptInfo:	""
						placeholderText:	"..."
						undoModel:			columnModel
						useTabAsSpaces:		false

						Layout.fillWidth:		true

					}
				}
			}


		}

		Rectangle
		{
			id: tabView

			visible:				columnModel.tabs.length > 0

			anchors.top:			common.bottom
			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			parent.bottom
			anchors.margins:		jaspTheme.generalAnchorMargin

			color: jaspTheme.uiBackground

			property var	currentTabButton
			property real	currentTabX:		currentTabButton ? currentTabButton.mapToItem(tabView, 0, 0).x : 0
			property real	currentTabWidth:	currentTabButton ? currentTabButton.width : 0

			QTC.TabBar
			{
				id: tabbar
				contentHeight:	tabBarHeight + tabButtonRadius
				width:			parent.width
				background: Rectangle { color: jaspTheme.uiBackground }

				property real	tabBarHeight:		28 * preferencesModel.uiScale
				property real	tabButtonRadius:	5 * preferencesModel.uiScale

				Repeater
				{
					id:		tabButtonRepeater
					model:	columnModel.tabs.length

					QTC.TabButton
					{
						id:			tabButton
						height:		tabbar.height
						width:		labelText.implicitWidth + 20 * preferencesModel.uiScale

						onCheckedChanged: if (checked) tabView.currentTabButton = tabButton

						background: Rectangle
						{
							color:			checked ? jaspTheme.uiBackground : jaspTheme.grayLighter
							radius:			tabbar.tabButtonRadius
							border.width:	1
							border.color:	checked ? jaspTheme.uiBorder : jaspTheme.borderColor
						}

						contentItem: Text
						{
							// The bottom of buttons are hidden to remove their bottom line with the radius
							// So the text has to be moved higher from the horizontal middle line.
							id:					labelText
							topPadding:			-tabbar.tabButtonRadius * 3/4
							text:				columnModel.tabs[index].title
							font:				jaspTheme.font
							color:				jaspTheme.black
							horizontalAlignment: Text.AlignHCenter
							verticalAlignment:	Text.AlignVCenter
							opacity:			checked ? 1 : .6
						}

						MouseArea
						{
							anchors.fill	: parent
							cursorShape		: checked ? Qt.ArrowCursor : Qt.PointingHandCursor
							acceptedButtons	: Qt.NoButton
						}
					}
				}
			}

			Rectangle
			{
				// This hides the bottom border of the buttons (with their radius)
				id		: roundingHider
				width	: tabbar.contentWidth
				height	: tabbar.tabButtonRadius + 1
				anchors
				{
					left:		parent.left
					top:		parent.top
					topMargin:	tabbar.tabBarHeight
				}
				color: jaspTheme.uiBackground
				z: 1

				Rectangle
				{
					// The Tabbar removes the left border. Redraw it.
					anchors.left:	parent.left
					anchors.top:	parent.top
					anchors.bottom: parent.bottom
					width:			1
					color:			jaspTheme.uiBorder
				}
			}

			Rectangle
			{
				// Rectangle to draw the border under the tabbar
				id: borderView
				anchors
				{
					fill:		parent
					topMargin:	tabbar.tabBarHeight
				}
				z:				1
				border.width:	1
				border.color:	jaspTheme.uiBorder
				color:			"transparent"
			}

			Rectangle
			{
				// Hide the line onder the active tab
				z:				1
				height:			1
				width:			tabView.currentTabWidth - 2
				color:			jaspTheme.uiBackground
				x:				tabView.currentTabX + 1
				anchors.top:	borderView.top
			}

			StackLayout
			{
				id: stack
				property var componentIndex:
				{
					"computed": 0,
					"label" : 1,
					"missingValues" : 2
				}
				currentIndex: tabbar.currentIndex >= 0 ? componentIndex[columnModel.tabs[tabbar.currentIndex].name] : -1

				anchors.top: tabbar.bottom
				anchors.bottom: parent.bottom
				anchors.left:	parent.left
				anchors.right:	parent.right
				anchors.margins: jaspTheme.generalAnchorMargin * 0.5
				anchors.topMargin: jaspTheme.generalAnchorMargin * 0.25

				ComputeColumnWindow
				{
					id: computedColumnWindow
				}

				LabelEditorWindow
				{
					id: labelEditonWindow
				}

				Rectangle
				{
					id:			missingValuesView
					color:		jaspTheme.uiBackground
					enabled:	!columnModel.isVirtual

					CheckBox
					{
						id:					useCustomValues
						label:				qsTr("Use custom values")
						checked:			columnModel.useCustomEmptyValues
						onCheckedChanged:	columnModel.useCustomEmptyValues = checked
					}

					PrefsMissingValues
					{
						id:					missingValues
						height:				missingValuesView.height - y
						anchors.top:		useCustomValues.bottom
						anchors.topMargin:	jaspTheme.generalAnchorMargin
						anchors.left:		parent.left
						anchors.leftMargin:	jaspTheme.generalAnchorMargin
						enabled:			useCustomValues.checked
						showTitle:			false
						model:				columnModel
						resetButtonTooltip: qsTr("Reset missing values with the ones set in your workspace")

					}

					Text
					{
						anchors.top:		parent.top
						anchors.left:		missingValues.right
						text:				qsTr("Change workspace missing values")
						color:				jaspTheme.blueDarker
						MouseArea
						{
							anchors.fill:	parent
							onClicked:		mainWindowRoot.showWorkspaceMenu()
							cursorShape:	Qt.PointingHandCursor
						}
					}
				}
			}
		}
	}
}


