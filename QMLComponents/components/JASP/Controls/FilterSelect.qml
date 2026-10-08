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
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import JASP.Controls

/*!
    \qmltype FilterSelect
    \inqmlmodule JASP.Controls
    \brief Dataset/filter selection dropdown for a host component with its own dataset selection.

    A DropDown bound to the option named by \c host.dataSetSelectionOption; its value is a FILTER
    ID (every filter of every workspace dataset is offered, labelled "DataSet - Filter"). Any
    component that consumes variable info for itself (VariablesForm, TextAreaBase, ...) embeds
    one, typically behind a <c>Loader { active: host.selectionAvailable }</c> so that a bound
    control without a name never exists at all.

    The host owns the selection state (HostFilterSelection); this control only mirrors it
    (host -> dropdown) and reports user picks back (dropdown -> host). Because the dropdown is a
    normal bound control, the selection reaches the analysis' options in desktop and syntax mode
    through exactly the same mechanism as every other option.
*/
DropDown
{
	id: filterSelect

	///< Any control with a HostFilterSelection (VariablesFormBase, TextAreaBase, ...).
	property QtObject host

	name:	 host && host.dataSetSelectionOption ? host.dataSetSelectionOption : ""
	title:	 qsTr("Data")
	toolTip: qsTr("Select the dataset (and filter) used by this component")
	values:	 host ? host.filterSelectionValues : []

	function _entryForFilterId(filterId)
	{
		const entries = filterSelect.values
		for (var i = 0; i < entries.length; i++)
			if (parseInt(entries[i].value) === filterId)
				return entries[i]
		return null
	}

	function syncFromHost()
	{
		if (!host || host.selectedFilterId < 0)
			return

		if (parseInt(currentValue) !== host.selectedFilterId)
		{
			const entry = _entryForFilterId(host.selectedFilterId)
			if (entry)
				currentValue = entry.value
		}
	}

	Connections
	{
		target: filterSelect.host

		function onSelectedFilterIdChanged()	{ filterSelect.syncFromHost() }
		function onFilterSelectionValuesChanged() { filterSelect.syncFromHost() }
	}

	//User picks (and bindTo of a saved/bridge value) land on the host; the host rejects ids
	//that name no filter, and syncFromHost() pulls the dropdown back to what it kept.
	onCurrentValueChanged:
	{
		const filterId = parseInt(currentValue)

		if (host && !isNaN(filterId) && filterId !== host.selectedFilterId)
			host.selectedFilterId = filterId
	}

	Component.onCompleted:	syncFromHost()
}
