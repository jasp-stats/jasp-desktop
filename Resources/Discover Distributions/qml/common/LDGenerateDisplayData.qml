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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Section
{
	property string distributionName
	property string distributionSimpleName		: distributionName
	property string formula
	property bool	histogramIsBarPlot			: false
	property bool	showStatisticsMoment		: true
	property bool	showCumulativeDistribution	: true
	property bool	allowOnlyScaleColumns		: true
	property bool	suggestScaleColumns			: false

	title: enabled ? qsTr("Generate and Display Data") : qsTr("Generate and Display Data") + " - " + qsTr("[requires a loaded data set]")
	Group
	{
		Layout.columnSpan: 2
		title: qsTr("Generate new variable from %1").arg(distributionName) + " (" + formula + ")"

		AddColumnField	{ name: "newVariableName"; text: qsTr("Variable name: "); fieldWidth: 120; placeholderText: qsTr("e.g., random %1").arg(distributionSimpleName) }
		IntegerField	{ name: "sampleSize"; label: qsTr("Number of samples: "); min: 1; defaultValue: dataSetModel.rowCount(); max: dataSetModel.rowCount() }
		Button
		{
			id: simulateNowButton
			name: "simulateNowButton"
			label: qsTr("Draw samples")
			onClicked: if (simulateNow.checked) simulateNow.checked = false; else simulateNow.checked = true
		}
		CheckBox { name: "simulateNow"; visible: false; id: simulateNow }
	}

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		visible: true
		AvailableVariablesList { name: "allVariables" }
		AssignedVariablesList
		{
			name: "variable"; label: qsTr("Get variable from data set");
			allowedColumns: allowOnlyScaleColumns ? ["scale"] : []
			suggestedColumns: suggestScaleColumns ? ['scale'] : []
			singleVariable: true
		}
	}

	Group
	{
		title: qsTr("Statistics")
		CheckBox{ name: "summary"; label: qsTr("Descriptives"); checked: true  }
		CheckBox
		{
			visible: showStatisticsMoment
			name: "moments"; label: qsTr("First"); childrenOnSameRow: true
			IntegerField{name: "momentsUpTo"; afterLabel: qsTr("observed moments"); defaultValue: 2; min: 1; max: 10}
		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "histogram";  label: histogramIsBarPlot ? qsTr("Bar plot") : qsTr("Histogram with"); childrenOnSameRow: true
			IntegerField { visible: !histogramIsBarPlot; name: "histogramBins"; afterLabel: qsTr("bins"); defaultValue: 30 }
		}
		CheckBox{ visible: showCumulativeDistribution; name: "ecdf"; label: qsTr("Empirical cumulative distribution") }
	}
}
