import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Controls	as JaspControls
import JASP


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

	// Returns whether the code was applied (an incomplete constructor formula is not).
	function applyComputedColumn()
	{
		if(!columnModel.column)
			return false

		if(isRCode)
			columnModel.setComputedColumnCode(computeColumnEdit.text, columnModel.column.constructorJson)
		else
		{
			computedColumnConstructor.forceActiveFocus();
			if(!computedColumnConstructor.checkAndApply())
				return false

			columnModel.setComputedColumnCode(computedColumnConstructor.rCode, computedColumnConstructor.returnFilterJSON())
		}

		return true
	}

	// Asks what to do with unapplied edits (the dialog is blocking) and returns whether the window
	// may close: not after Cancel, nor when the edits could not be applied.
	function askIfChangedOrClose()
	{
		if(!(columnModel.column && columnModel.column.isComputed && columnModel.computedTypeEditable && computedColumnContainer.changed))
			return true

		saveDialog.open()
		return saveDialog.resolved
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
				bottom:			computeRefWarning.top
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
				mode:					ScriptConstructorMode.ComputedColumn
				anchors.fill:			parent
				anchors.leftMargin:		1
				visible:				!isRCode
				deferUntilVisible:		true

				showGeneratedRCode:		false
				KeyNavigation.tab:		applyComputedColumnButton

				// the constructor palette gains outward column sourcing too (this
				//supersedes the v1 own-dataset palette cut for the constructor only - the
				//emitted code is visible R, so the full grammar applies). The context is the
				//column's COMPUTE FILTER - the same filter the run encodes the output name and
				//reads the slice through, and the same context the R editor's picker
				//uses.
				contextFilterId:		columnModel.column
										? dataSetPackage.workspace.filterIdByName(dataSetPackage.workspace.shownDataSetId(), columnModel.column.computeFilter)
										: -1
			}

		}

		// live validation of the qualified references in the R code as typed
		// (the scanner's first typed error). Non-blocking: it warns, the run itself still
		// fails loudly at the engine when a reference cannot resolve there.
		Text
		{
			id:					computeRefWarning
			visible:			isRCode && computeRefValidator.error !== ""
			height:				visible ? contentHeight + 4 * jaspTheme.uiScale : 0

			anchors
			{
				left:			parent.left
				right:			parent.right
				bottom:			computeColumnErrorScroll.top
				leftMargin:		2 * jaspTheme.uiScale
				rightMargin:	2 * jaspTheme.uiScale
				bottomMargin:	visible ? 2 * jaspTheme.uiScale : 0
			}

			wrapMode:	Text.Wrap
			font:		jaspTheme.fontCode
			color:		jaspTheme.controlWarningTextColor
			text:		qsTr("Note: ") + computeRefValidator.error
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
				id:				insertColumnRefButton
				visible:		isRCode
				width:			visible ? height : 0

				toolTip:		qsTr("Insert a column reference from another dataset or filter")
				iconSource:		jaspTheme.iconPath + "round_addition.png"

				anchors.left:	showGeneratedRCode.right
				anchors.bottom:	parent.bottom
				anchors.top:	helpButton.top

				onClicked:		computeRefPicker.openForInsertion()
			}

			JaspControls.RectangularButton
			{
				id:					applyComputedColumnButton

				text:				qsTr("Compute column") 
				anchors.left:		insertColumnRefButton.right
				anchors.right:		computeFilterDropDown.left
				centerTextParent:	true
				anchors.bottom:		parent.bottom
				anchors.top:		helpButton.top
				onClicked:			{ forceActiveFocus(); computedColumnContainer.applyComputedColumn() }
				toolTip:			qsTr("Click to compute column")
			}
			
			// the computeFilterDropDown is THE fallback encoder selection for
			// bare names in this editor (the "computed column | selected input filter" row):
			// the picker/validator below resolve their context through it, and the
			// run resolves the same way (EngineSync::computeColumn -> engine ->
			// compCol->computeFilter()'s view). Switching it applies the CURRENT editor text
			// first and then re-queues: both sends re-scan their code at queue time, so
			// this can never run a stale referencedFilterIds sidecar.
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

		// outward-reference support for the R code editor (the constructor keeps
		// its v1 own-dataset palette). The insertion context is the column's COMPUTE FILTER's
		// id - the same filter the run encodes the output name and reads the slice through
		// - so the picker's bare candidates are exactly the columns the bare names in
		// this code resolve to at run time. The validator is debounced and
		// read-only: it warns, it never triggers a run.
		JaspControls.QualifiedColumnValidator
		{
			id:			computeRefValidator
			workspace:	dataSetPackage.workspace
			target:		computeColumnEdit
		}

		JaspControls.QualifiedColumnPicker
		{
			id:					computeRefPicker
			target:				computeColumnEdit
			workspace:			dataSetPackage.workspace
			contextFilterId:	columnModel.column
								? dataSetPackage.workspace.filterIdByName(dataSetPackage.workspace.shownDataSetId(), columnModel.column.computeFilter)
								: -1
		}

		SaveDiscardCancelDialog
		{
			id:			saveDialog
			title:		qsTr("Would you like to save your changes to the Computed Column?")
			text:		qsTr("Your changes will be lost if you don't save them.")

			// Whether the last answer dealt with the edits (applied or discarded them); see askIfChangedOrClose().
			property bool resolved: false

			onCancel:	resolved = false
			onSave:		resolved = computedColumnContainer.applyComputedColumn()
			onDiscard:
			{
				resolved = true

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
