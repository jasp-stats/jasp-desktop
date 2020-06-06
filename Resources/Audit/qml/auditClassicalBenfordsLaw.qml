
// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

// When making changes to this file always mention @koenderks as a 
// reviewer in the Pull Request

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {

	usesJaspResults: true
	columns: 1

	VariablesForm
	{
		id: 						variablesFormBenfordsLaw
		preferredHeight:			jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList	
		{ 
			name: 				"variablesFormBenfordsLaw" 
		}

		AssignedVariablesList
		{
			id: 						values
			name: 					"values"
			title: 					qsTr("Variable")
			singleVariable:	true
			allowedColumns:	["ordinal", "scale"]
		}

		DropDown {
				name: 							"digits"
				indexDefaultValue: 	0
				label: 							qsTr("Check numbers:")
				values:
				[
						{ label: "1 - 9", value: "first"},
						{ label: "10 - 99", value: "firstSecond"},
				]
		}
	}

	Section 
	{
		text: qsTr("Advanced Options")

		GridLayout 
		{
			columns: 3

			PercentField
			{
				name: 				"confidence"
				label: 				qsTr("Confidence")
				decimals: 		2
				defaultValue: 95
			}

			GroupBox 
			{
				title: qsTr("Explanatory Text")

				RowLayout 
				{
					CheckBox 
					{
						id: 		explanatoryText
						text:	 	qsTr("Enable")
						name: 		"explanatoryText"
						checked: 	true
					}

					HelpButton 
					{ 
						helpPage:			"Audit/explanatoryText"
						toolTip: 			qsTr("Show explanatory text and formulas")
					}
				}
			}
		}
	}

	Section {
		title: qsTr("Tables and Plots")

		GridLayout 
		{
			columns: 2

			GroupBox
			{
				title: qsTr("Tables")

				CheckBox
				{
					text: qsTr("Descriptive statistics")
					name: "summaryTable"
					checked: true
				}
			}

			GroupBox
			{
				title: qsTr("Plots")

				CheckBox
				{
					text: qsTr("Compare vs. Benford's law")
					name: "benfordsLawPlot"
				}
			}
		}
	}

	Item 
	{
		Layout.preferredHeight: downloadReportBenfordsLaw.height
		Layout.fillWidth: 			true

		Button 
		{
			id: 							downloadReportBenfordsLaw
			enabled: 					values.count > 0
			anchors.right: 		parent.right
			anchors.bottom: 	parent.bottom
			text: 						qsTr("<b>Download Report</b>")

			onClicked: 
			{
				form.exportResults()
			}
		}
	}
}
