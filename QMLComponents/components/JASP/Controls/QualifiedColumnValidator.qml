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
import JASP

/*!
	\qmltype QualifiedColumnValidator
	\inqmlmodule JASP.Controls 1.0
	\brief Debounced editor-side validation of qualified column references.

	Watches an R editor's text and runs the qualified-reference scanner on it
	(\c Workspace::qualifiedRefErrorInCode): "" when every qualified reference in the code
	resolves against the live workspace, otherwise the first typed error message. The editors
	show \c error inline as a NON-BLOCKING warning - the run itself still fails loudly at the
	engine when a reference cannot resolve there; this is only the
	type-time feedback, so it stays cheap (debounced) and never triggers a run.
*/
QtObject
{
	id: qualifiedColumnValidator

	///< The Workspace the validation resolves against.
	property var		workspace: null
	///< The editor TextArea whose text is validated (a QtQuick.Controls TextArea).
	property var		target: null
	///< Debounce for the scan while the user types (the scan is cheap but per-keystroke
	///  re-resolution of a big workspace is not free).
	property int		debounceInterval: 400
	///< Set false to suspend validation without disconnecting the editor.
	property bool		active: true
	///< The first typed error of the qualified-reference scanner ("" = clean, nothing to warn about).
	property string		error: ""

	function validateNow()
	{
		error = (active && workspace && target) ? workspace.qualifiedRefErrorInCode(target.text) : ""
	}

	onTargetChanged:	validateNow()
	onWorkspaceChanged:	validateNow()
	onActiveChanged:	validateNow()

	property Timer _debounce: Timer
	{
		interval:	qualifiedColumnValidator.debounceInterval
		onTriggered: qualifiedColumnValidator.validateNow()
	}

	property Connections _targetWatcher: Connections
	{
		target: qualifiedColumnValidator.target

		function onTextChanged()
		{
			if(qualifiedColumnValidator.active)
				qualifiedColumnValidator._debounce.restart()
		}
	}

	// cross-dataset dependency invalidation, editor side. A reference that was
	//valid when typed can be invalidated WITHOUT any edit: the referenced dataset is
	//unloaded or retitled, a referenced filter is renamed or deleted, or a referenced
	//column is churned (renamed/removed by a sync). The validator re-scans on each of those
	//lifecycle events (all relayed on the Workspace: dataSetRemoved / dataSetTitleChanged /
	//filterNameChanged / filtersCountChanged / datasetChanged) so a previously-valid
	//reference turns invalid IN THE EDITOR - loud, inline, non-blocking (the run itself
	//still fails loudly at the engine with the same typed message; no tombstoning).
	property Connections _workspaceWatcher: Connections
	{
		target: qualifiedColumnValidator.workspace

		function onDataSetRemoved(dataSetId)				{ qualifiedColumnValidator.validateNow() }
		function onDataSetTitleChanged(dataSetId)			{ qualifiedColumnValidator.validateNow() }
		function onFilterNameChanged(dataSetId)				{ qualifiedColumnValidator.validateNow() }
		function onFiltersCountChanged()					{ qualifiedColumnValidator.validateNow() }
		function onDatasetChanged(dataSetId, changedColumns, missingColumns, changeNameColumns, rowCountChanged, hasNewColumns)
		{
			qualifiedColumnValidator.validateNow()
		}
	}
}
