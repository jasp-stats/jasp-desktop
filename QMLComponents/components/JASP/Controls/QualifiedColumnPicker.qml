//
// Copyright (C) 2013-2026 University of Amsterdam
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
import QtQuick.Controls
import QtQuick.Layouts
import JASP

/*!
	\qmltype QualifiedColumnPicker
	\inqmlmodule JASP.Controls 1.0
	\brief Picker/autocomplete for outward multi-slice column references.

	A popup fed by \c Workspace::qualifiedColumnCandidates(contextFilterId): every column of
	every filter of every loaded dataset, rendered by \c Workspace::renderQualifiedColumnRef.
	The context filter's own columns come out as the bare names (that namespace is the
	fallback for unqualified names - the editor's own dataset for the filter editor, the
	selected compute/input filter for the compute editors); every other slice comes out as the
	minimal qualified surface form ("DS"."Col", "F"."Col" when the filter name is unique, the
	full form otherwise). Insertion puts the rendered code VERBATIM at the target's cursor:
	stored code stays raw (the column.cpp:547 contract) - encoding to tokens happens only at
	run, engine-side.

	The editors instantiate this once per R editor and open it from an "insert reference"
	button; the search field gives the type-to-filter autocomplete behaviour.
*/
Popup
{
	id: qualifiedColumnPicker

	///< The editor TextArea the reference is inserted into (a QtQuick.Controls TextArea).
	property var		target: null
	///< The bare-name fallback namespace: the edited filter (FilterWindow), the selected
	///  compute filter (ComputeColumnWindow) or the selected input filter (ComputeDataSetPanel).
	///  -1 = no context: every candidate renders qualified.
	property int		contextFilterId: -1
	///< The Workspace the catalog comes from.
	property var		workspace: null
	///< The candidate list, {code, label, dataSetTitle, filterName, filterTitle, columnName,
	///  filterId, isBare} entries; rebuilt from the workspace on every open.
	property var		candidates: []

	width:				420 * jaspTheme.uiScale
	height:				340 * jaspTheme.uiScale
	padding:			1 * jaspTheme.uiScale
	modal:				false
	closePolicy:		Popup.CloseOnEscape | Popup.CloseOnPressOutsideParent

	background: Rectangle
	{
		color:			jaspTheme.uiBackground
		border.color:	jaspTheme.uiBorder
		border.width:	1
		radius:			4
	}

	property var visibleCandidates:
	{
		const needle = searchField.text.toLowerCase()

		if(!needle.length)
			return candidates

		return candidates.filter(function(candidate)
		{
			return candidate.label.toLowerCase().indexOf(needle) >= 0
				|| candidate.code.toLowerCase().indexOf(needle)  >= 0
		})
	}

	onAboutToShow:	rebuildCandidates()
	onClosed:		if(target) target.forceActiveFocus()

	///Rebuild the candidate list from the workspace (fresh every open: datasets, filters and
	///columns can have changed since last time - the catalog is never cached).
	function rebuildCandidates()
	{
		candidates = workspace ? workspace.qualifiedColumnCandidates(contextFilterId) : []
	}

	///The insertion primitive the editors share: the rendered surface form goes in verbatim at
	///the target's cursor. Returns whether something was inserted.
	function insertCandidate(candidate)
	{
		if(!target || !candidate || !candidate.code || candidate.code.length === 0)
			return false

		const position = target.cursorPosition

		target.insert(position, candidate.code)
		target.cursorPosition = position + candidate.code.length //keep the caret after what we inserted
		target.forceActiveFocus()

		return true
	}

	///Open the picker near the target's cursor (the autocomplete position).
	function openForInsertion()
	{
		rebuildCandidates()
		searchField.text = ""

		if(target && target.visible)
		{
			//Just above the cursor rectangle, like a normal autocomplete popup; clamped into
			//the window so a cursor at the screen edge cannot push the popup outside.
			const cursorGlobal = target.mapToItem(null, 0, target.cursorRectangle.y)
			const windowWidth  = target.Window ? target.Window.width : 800

			x = Math.max(0, Math.min(cursorGlobal.x, windowWidth - width))
			y = Math.max(0, cursorGlobal.y - height - 4 * jaspTheme.uiScale)
		}

		open()
		searchField.forceActiveFocus()
	}

	contentItem: ColumnLayout
	{
		spacing: 4 * jaspTheme.uiScale

		Text
		{
			Layout.fillWidth:		true
			text:					qsTr("Insert a column reference")
			font:					jaspTheme.fontGroupTitle
			color:					jaspTheme.textEnabled
			elide:					Text.ElideRight
		}

		TextField
		{
			id:						searchField
			objectName:				"qualifiedColumnPickerSearchField"
			Layout.fillWidth:		true
			placeholderText:		qsTr("Type to filter, Enter inserts the selected reference")
			font:					jaspTheme.font
			color:					jaspTheme.textEnabled
			selectByMouse:			true

			function insertSelected()
			{
				if(!visibleCandidates.length)
					return

				qualifiedColumnPicker.insertCandidate(visibleCandidates[Math.max(listView.currentIndex, 0)])
				qualifiedColumnPicker.close()
			}

			Keys.onDownPressed:		(event) => {
										if(visibleCandidates.length)
										{
											listView.forceActiveFocus()
											listView.currentIndex = Math.max(listView.currentIndex, 0)
											event.accepted = true
										}
									}
			Keys.onReturnPressed:	(event) => { searchField.insertSelected(); event.accepted = true }
			Keys.onEnterPressed:	(event) => { searchField.insertSelected(); event.accepted = true }
		}

		ListView
		{
			id:						listView
			objectName:				"qualifiedColumnRefsList"
			Layout.fillWidth:		true
			Layout.fillHeight:		true
			clip:					true
			model:					visibleCandidates
			keyNavigationEnabled:	true
			spacing:				2 * jaspTheme.uiScale

			//JASPScrollBar is a plain Item driven by a Flickable, not a QQuickScrollBar, so it
			//cannot attach through ScrollBar.vertical: overlay it next to the list, like
			//ComponentsList does.
			JASPScrollBar
			{
				flickable:		listView
				vertical:		true
				z:				2

				anchors
				{
					top:		parent.top
					right:		parent.right
					bottom:		parent.bottom
					margins:	2 * jaspTheme.uiScale
				}
			}

			delegate: ItemDelegate
			{
				width:				listView.width
				highlighted:		ListView.isCurrentItem
				focus:				false

				onClicked:
				{
					qualifiedColumnPicker.insertCandidate(modelData)
					qualifiedColumnPicker.close()
				}

				contentItem: ColumnLayout
				{
					spacing: 0

					Text
					{
						Layout.fillWidth:	true
						text:				modelData.label
						font:				jaspTheme.font
						color:				jaspTheme.textEnabled
						elide:				Text.ElideRight
					}
					Text
					{
						Layout.fillWidth:	true
						text:				modelData.code
						font:				jaspTheme.fontCode
						color:				jaspTheme.textDisabled
						elide:				Text.ElideRight
					}
				}
			}

			Text
			{
				anchors.centerIn:	parent
				visible:			visibleCandidates.length === 0
				text:				qsTr("No matching column reference")
				font:				jaspTheme.font
				color:				jaspTheme.textDisabled
			}
		}
	}
}
