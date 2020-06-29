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
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3

Section
{
	title:		qsTr("MCMC diagnostics")

	VariablesForm
	{
		preferredHeight: 200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:	"availableModelComponentsDiagnostics"
			title:	qsTr("Model terms")
			source:	"fixedEffects"
		}

		AssignedVariablesList
		{
			singleVariable:	true
			name:			"samplingVariable1"
			title:			if(samplingPlot.currentValue == "stan_scat"){qsTr("Horizontal axis")}else{qsTr("Plotted term")}
		}

		AssignedVariablesList
		{
			singleVariable:	true
			name:			"samplingVariable2"
			title:			qsTr("Vertical axis")
			visible:		samplingPlot.currentValue == "stan_scat"
			onVisibleChanged: if (!visible && count > 0) itemDoubleClicked(0)
		}
	}

	DropDown
	{
		name:	"samplingPlot"
		id:		samplingPlot
		label:	qsTr("Plot type")
		values:
		[
			{ label: qsTr("Traceplot"),			value: "stan_trace"},
			{ label: qsTr("Scatterplot"),		value: "stan_scat"},
			{ label: qsTr("Histogram"),			value: "stan_hist"},
			{ label: qsTr("Density"),			value: "stan_dens"},
			{ label: qsTr("Autocorrelations"),	value: "stan_ac"}
		]
	}
}


