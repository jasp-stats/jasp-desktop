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

Form
{
	usesJaspResults: true
	
	IntegerField { visible: false; name: "plotWidthQQPlot"                      ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightQQPlot"                     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotLegend"     ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"   ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotLegend"      ; defaultValue: 430 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"    ; defaultValue: 350 }
	
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "dependent";		title: qsTr("Dependent Variables");	suggestedColumns: ["scale"]; singleVariable: false		}
		AssignedVariablesList { name: "fixedFactors";	title: qsTr("Fixed Factors");		suggestedColumns: ["ordinal", "nominal"]				}
		AssignedVariablesList { name: "randomFactors";	title: qsTr("Random Factors");		suggestedColumns: ["ordinal", "nominal"];	debug: true }
	}
	
	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors"] }
			AssignedVariablesList {  name: "modelTerms"; title: qsTr("Model Terms"); listViewType: "Interaction" }
		}
		CheckBox { name: "includeIntercept";		label: qsTr("Include intercept"); checked: true }
		
	}
	
	
	Section
	{
		title: qsTr("Additional Options")
		
		Group
		{
			title: qsTr("Test")
			CheckBox { name: "testPillai";		label: qsTr("Pillai"); checked: true }
			CheckBox { name: "testWilks";		label: qsTr("Wilks"); checked: false }
			CheckBox { name: "testHotellingLawley";	label: qsTr("Hotelling-Lawley"); checked: false }
			CheckBox { name: "testRoy";		label: qsTr("Roy"); checked: false }
		}
		
		Group
		{
			title: qsTr("Assumption Checks")
			CheckBox { name: "boxMTest";			label: qsTr("Homogeneity of covariance matrices")}
			CheckBox { name: "shapiroTest";			label: qsTr("Multivariate normality")		}
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox { name: "includeAnovaTables"; label: qsTr("ANOVA tables") }
			CheckBox { name: "VovkSellkeMPR"; label: qsTr("Vovk-Sellke maximum p-ratio") }
		}

	
	}
	
}
