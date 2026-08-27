import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Controls	as JaspControls
import JASP

import "FilterConstructor"

FocusScope
{
	id:				computedColumnContainer

	property bool	isRCode:					columnModel.column && columnModel.column.codeType == computedColumnTypeRCode
	property bool	changed:					isRCode ? computeColumnEdit.changed : computedColumnConstructor.somethingChanged
	property int	minimumHeightTextBoxes:		50 * preferencesModel.uiScale
	property real	desiredMinimumHeight:		computeColumnButtons.height + computeColumnErrorScroll.height + (isRCode ? computeColumnEditRectangle.desiredMinimumHeight : computedColumnConstructor.desiredMinimumHeight)

	// The C++ constructor only builds its chrome/palettes when it is actually visible.
	// StackLayout sets our visibility for tab switches; this forwards it so the deferred
	// build runs the first time the computed column tab is shown.
	onVisibleChanged:			if(visible) computedColumnConstructor.requestBuild()

	Connections
	{
		target: columnModel.column

		function onConstructorJsonChanged()
		{
			computedColumnConstructor.initializeFromJSON(/*columnModel.column.codeType == computedColumnTypeRCode ? "{\"formulas\":[]}" :*/ columnModel.column.constructorJson);
		}

		function onRCodeChanged()
		{
			computeColumnEdit.text = columnModel.column.rCode;
		}
	}
	
	Connections
	{
		target: columnModel

		function onChosenColumnChanged()
		{
			if(columnModel.column && columnModel.column.codeType == computedColumnTypeRCode)
				computeColumnEdit.text = columnModel.column.rCode;
			else if(columnModel.column)
				computedColumnConstructor.initializeFromJSON(columnModel.column.constructorJson);
		}
	}

	Rectangle
	{
		id:				computedColumnWindowBackground
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		anchors.fill:	parent
		z:				-1
	}

	function applyComputedColumn()
	{
		if(!columnModel.column)
			return

		if(isRCode)
			columnModel.setComputedColumnCode(computeColumnEdit.text, columnModel.column.constructorJson)
		else
		{
			computedColumnConstructor.forceActiveFocus();
			if(computedColumnConstructor.checkAndApply())
				columnModel.setComputedColumnCode(computedColumnConstructor.rCode, computedColumnConstructor.returnFilterJSON())
		}
	}

	function askIfChangedOrClose()
	{
		if(columnModel.column && columnModel.column.isComputed && columnModel.computedTypeEditable && computedColumnContainer.changed)	
			saveDialog.open()
	}

	Item
	{
		id:				minWidthCollector

		property int minWidth: 400 * preferencesModel.uiScale

		anchors
		{
			top:			parent.top
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			rightMargin:	Math.min(0, computedColumnContainer.width - minWidthCollector.minWidth)
		}

		Item
		{
			id: computeColumnCodeArea

			anchors
			{
				top:			parent.top
				bottom:			computeColumnErrorScroll.top
				left:			parent.left
				right:			parent.right
				topMargin:		1
				leftMargin:		anchors.topMargin
				rightMargin:	anchors.topMargin
			}

			Rectangle
			{
				id:		computeColumnEditRectangle
				color: jaspTheme.white

				border.width: 1
				border.color: jaspTheme.grayLighter

				property real desiredMinimumHeight: computedColumnContainer.minimumHeightTextBoxes

				visible: isRCode

				anchors.fill: parent

				TextArea
				{
					id: computeColumnEdit
					
					JaspControls.RSyntaxHighlighterQuick
					{
						textDocument:		computeColumnEdit.textDocument
					}
					

					anchors.top:			parent.top
					anchors.left:			parent.left
					anchors.right:			parent.right
					height:					Math.max(contentHeight + 30, parent.height - 10)
					selectByMouse:			true
					onActiveFocusChanged:	if(!activeFocus) deselect()
					placeholderText:		"Enter your R code here"
					font:					jaspTheme.fontRCode
					wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere
					color:					jaspTheme.textEnabled

					property bool changedSinceLastApply:	text !== computedColumnContainer.lastAppliedcomputeColumn
					property bool changed:					columnModel.column && text !== columnModel.column.rCode
					
					KeyNavigation.tab:		applyComputedColumnButton


					Keys.onReturnPressed:	(keyEvent) => {
												if(keyEvent.modifiers & Qt.ControlModifier)
												{
													if(changedSinceLastApply)
														computedColumnContainer.applyComputedColumn()
												}
												else
													keyEvent.accepted = false
											}
				}


				Image
				{
					id:							backgroundImage

					source:						jaspTheme.iconPath + "/columnConstructorBackground.png"
					anchors.centerIn:			parent

					property real widthScale:	parent.width  / implicitWidth
					property real heightScale:	parent.height / implicitHeight
					property real ratio:		Math.min(Math.min(widthScale, heightScale), 1.0) * 0.5

					width:						implicitWidth * ratio
					height:						implicitHeight * ratio
				}
			}

			ScriptConstructor
			{
				id:						computedColumnConstructor
				mode:					ScriptConstructor.ComputedColumn
				anchors.fill:			parent
				anchors.leftMargin:		1
				visible:				!isRCode
				deferUntilVisible:		true
			
				showGeneratedRCode:		false
				KeyNavigation.tab:		applyComputedColumnButton
			}

		}

		ScrollView
		{
			id: computeColumnErrorScroll
			height: visible ? computedColumnContainer.minimumHeightTextBoxes : 0

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: computeColumnButtons.top

			visible: computeColumnError.text.length > 0

			TextArea
			{
				id:						computeColumnError
				color:					jaspTheme.red
				readOnly:				true
				text:					!columnModel.column ? "" : columnModel.column.error

				selectByMouse:			true
				onActiveFocusChanged:	if(!activeFocus) deselect()

				font:					jaspTheme.fontCode
				height:					text.length === 0 ? 0 : computeColumnError.contentHeight
			}
		}

		Item
		{
			id:				computeColumnButtons
			height:			helpButton.height
			anchors
			{
				left:		parent.left
				right:		parent.right
				bottom:		parent.bottom
				margins:	1
			}

			JaspControls.RectangularButton
			{
				id:				showGeneratedRCode
				visible:		!isRCode
				width:			visible ? implicitWidth : 0

				toolTip:		qsTr("Show generated R code")
				iconSource:		jaspTheme.iconPath + "/R.png"

				anchors.left:	parent.left
				anchors.bottom:	parent.bottom
				anchors.top:	helpButton.top

				onClicked:		computedColumnConstructor.showGeneratedRCode = !computedColumnConstructor.showGeneratedRCode
			}
		
			JaspControls.RectangularButton
			{
				id:					applyComputedColumnButton

				text:				qsTr("Compute column") 
				anchors.left:		showGeneratedRCode.right
				anchors.right:		computeFilterDropDown.left
				centerTextParent:	true
				anchors.bottom:		parent.bottom
				anchors.top:		helpButton.top
				onClicked:			{ forceActiveFocus(); computedColumnContainer.applyComputedColumn() }
				toolTip:			qsTr("Click to compute column")
			}
			
			JaspControls.DropDown
			{
				id:					computeFilterDropDown
				values:				filterModel.computeFilterDropDownList
				startValue:			""
				currentValue:		columnModel.column ? columnModel.column.computeFilter : -1
				onValueChanged:		{
					computedColumnContainer.applyComputedColumn()
					columnModel.setComputeFilterQ(currentValue)
					
				}
				anchors.right:		helpButton.left
				anchors.bottom:		parent.bottom
				toolTip:			qsTr("Select a filter to use for this computed column")
				control.height:		applyComputedColumnButton.height
			}

			JaspControls.HelpButton
			{
				id:				helpButton
				anchors.right:	parent.right
				anchors.bottom: parent.bottom
				helpMD:			allHelp.computedcolumns
				toolTip:		qsTr("Open Documentation")
				height:			33 * jaspTheme.uiScale
				width:			height
				buttonPadding:  6 * preferencesModel.uiScale
			}
		}

		SaveDiscardCancelDialog
		{
			id:			saveDialog
			title:		qsTr("Would you like to save your changes to the Computed Column?")
			text:		qsTr("Your changes will be lost if you don't save them.")
			onSave:
			{
				computedColumnContainer.applyComputedColumn()
			}
			onDiscard:
			{
				//Revert any unsaved edits back to whatever is stored on the column.
				if(columnModel.column)
				{
					if(columnModel.column.codeType == computedColumnTypeRCode)
						computeColumnEdit.text = columnModel.column.rCode;
					else
						computedColumnConstructor.initializeFromJSON(columnModel.column.constructorJson);
				}
			}
		}
	}
}
