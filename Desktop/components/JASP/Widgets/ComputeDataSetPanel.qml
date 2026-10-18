import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Controls	as JaspControls
import JASP

// Bottom-of-datapanel editor for computed datasets. Only visible when the shown
// dataset is a computed (R) dataset. The R code's last expression must be a
// data.frame; it is written back into the output dataset via .setDataSet.
Rectangle
{
	id:							root

	readonly property var		workspace:			dataSetPackage.workspace
	readonly property var		shownDataSet:		workspace.shownDataSet

	property bool				expanded:			true

	property real				headerHeight:		35 * preferencesModel.uiScale
	property real				contentHeight:		220 * preferencesModel.uiScale

	height:						expanded ? headerHeight + contentHeight : headerHeight

	color:						jaspTheme.uiBackground
	border.color:				jaspTheme.grayLighter
	border.width:				1

	function syncFromShown()
	{
		if(shownDataSet)
			codeEdit.text = shownDataSet.rCode
	}

	function applyComputedDataSet()
	{
		if(!shownDataSet)
			return

		shownDataSet.rCode = codeEdit.text
	}

	Connections
	{
		target:					workspace

		function onShownDataSetChanged()
		{
			root.syncFromShown()
		}
	}

	Component.onCompleted:		syncFromShown()

	// ---------- header (always visible when computed) ----------
	RowLayout
	{
		id:						headerLayout
		anchors.left:			parent.left
		anchors.right:			parent.right
		anchors.top:			parent.top
		height:					root.headerHeight
		anchors.margins:		jaspTheme.generalAnchorMargin
		anchors.topMargin:		0

		Text
		{
			Layout.fillWidth:	true
			text:				qsTr("Computed dataset")
			font:				jaspTheme.fontGroupTitle
			color:				jaspTheme.textEnabled
			verticalAlignment:	Text.AlignTop
		}

		
		JaspControls.MenuButton
		{
			id:					toggleButton
			//text:				root.expanded ? qsTr("Hide") : qsTr("Show")
			onClicked:			root.expanded = !root.expanded
			iconSource:			jaspTheme.iconPath + "collapse.png"
			rotation:			root.expanded ? 180 : 0
			radius:				height
		}
	}

	// ---------- content ----------
	ColumnLayout
	{
		id:						contentColumn
		visible:				root.expanded
		anchors.top:			headerLayout.bottom
		anchors.left:			parent.left
		anchors.right:			parent.right
		anchors.bottom:			parent.bottom
		anchors.margins:		jaspTheme.generalAnchorMargin
		spacing:				6 * jaspTheme.uiScale

		RowLayout
		{
			Layout.fillWidth:	true

			Text
			{
				text:			qsTr("Input filter")
				font:			jaspTheme.font
				color:			jaspTheme.textEnabled
			}

			JaspControls.DropDown
			{
				id:				inputDropDown
				Layout.fillWidth: true
				values:			workspace.inputFilterDropDownList
				startValue:		""
				currentValue:	shownDataSet && shownDataSet.defaultInputFilterId >= 0 ? String(shownDataSet.defaultInputFilterId) : ""
				onValueChanged:
				{
					if(shownDataSet && inputDropDown.currentValue.length > 0)
					{
						var filterId = parseInt(inputDropDown.currentValue)
						if(!isNaN(filterId) && filterId >= 0)
							shownDataSet.defaultInputFilterId = filterId
					}
				}
			}
		}

		Text
		{
			text:				qsTr("R code (produces a data.frame)")
			font:				jaspTheme.font
			color:				jaspTheme.textEnabled
		}

		Rectangle
		{
			id:					codeEditRectangle
			Layout.fillWidth:	true
			Layout.fillHeight:	true
			color:				jaspTheme.white
			border.color:		jaspTheme.grayLighter
			border.width:		1

			TextArea
			{
				id:					codeEdit
				anchors.fill:		parent
				anchors.margins:	2
				placeholderText:	qsTr("data.frame(x = someColumn * 2, y = anotherColumn)")
				selectByMouse:		true
				wrapMode:			TextArea.WrapAtWordBoundaryOrAnywhere
				color:				jaspTheme.textEnabled
				font:				jaspTheme.fontRCode

				JaspControls.RSyntaxHighlighterQuick
				{
					textDocument:	codeEdit.textDocument
				}
				
				Keys.onEnterPressed:  (event)=> {
										if((event.modifiers & Qt.ControlModifier) || (event.modifiers & Qt.MetaModifier))
										{
											applyButton.forceActiveFocus();
											root.applyComputedDataSet();
										}
									  }
			}
		}

		// mds-plan D7.2: live validation of the qualified references in the R code as typed
		// (the D7.3 scanner's first typed error). Non-blocking: it warns, the run itself still
		// fails loudly at the engine when a reference cannot resolve there (D7.5's surface).
		Text
		{
			id:					refWarning
			Layout.fillWidth:	true
			visible:			refValidator.error !== ""
			wrapMode:			Text.Wrap
			font:				jaspTheme.fontCode
			color:				jaspTheme.controlWarningTextColor
			text:				qsTr("Note: ") + refValidator.error
		}

		RowLayout
		{
			Layout.fillWidth:	true
			
			JaspControls.RectangularButton
			{
				id:				insertColumnRefButton
				text:			qsTr("Insert reference")
				toolTip:		qsTr("Insert a column reference from another dataset or filter")
				onClicked:		refPicker.openForInsertion()
			}

			JaspControls.RectangularButton
			{
				id:				applyButton
				text:			qsTr("Apply computed dataset")
				Layout.alignment: Qt.AlignRight
				onClicked:		root.applyComputedDataSet()
			}

			Text
			{
				id:				errorText
				Layout.fillWidth: true
				visible:		shownDataSet && shownDataSet.error.length > 0
				text:			shownDataSet ? shownDataSet.error : ""
				color:			jaspTheme.red
				font:			jaspTheme.fontCode
				wrapMode:		Text.Wrap
			}

			
		}
	}

	// mds-plan D7.2: outward-reference support for the computed-dataset R code. The
	// input-filter DropDown above is THE fallback encoder selection for bare names in this
	// editor (C13, "computed dataset | selected input filter" row of plan §4): the picker's
	// context is that selected input filter's id - the same filter the run reads the input
	// slice through (D7.3: EngineSync::computeDataSet -> engine resolves defaultInputFilterId)
	// - so its own-dataset columns come out of the picker as the bare names the bare names in
	// this code resolve to at run time. This is a plain dataset rCode context (no compute
	// filter of its own). The validator is debounced and read-only: it warns, it never
	// triggers a run.
	JaspControls.QualifiedColumnValidator
	{
		id:			refValidator
		workspace:	workspace
		target:		codeEdit
	}

	JaspControls.QualifiedColumnPicker
	{
		id:					refPicker
		target:				codeEdit
		workspace:			workspace
		contextFilterId:	shownDataSet && shownDataSet.defaultInputFilterId >= 0 ? shownDataSet.defaultInputFilterId : -1
	}
}
