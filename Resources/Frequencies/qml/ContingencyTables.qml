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
	usesJaspResults: false
	
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "rows";		title: qsTr("Rows");	suggestedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "columns";	title: qsTr("Columns");	suggestedColumns: ["ordinal", "nominal"] }
        AssignedVariablesList { name: "counts";		title: qsTr("Counts");	suggestedColumns: ["scale", "ordinal"]; singleVariable: true }
		AssignedVariablesList { name: "layers";		title: qsTr("Layers");	suggestedColumns: ["ordinal", "nominal"]; listViewType: "Layers"; height: 120 }
	}
	
	Section
	{
		title: qsTr("Statistics")
		
		Group
		{
			CheckBox { name: "chiSquared";						label: qsTr("χ²"); checked: true			}
			CheckBox { name: "chiSquaredContinuityCorrection";	label: qsTr("χ² continuity correction")	}
			CheckBox { name: "likelihoodRatio";					label: qsTr("Likelihood ratio")			}
		}

		Group
		{
			CheckBox
			{
				name: "oddsRatio"; label: qsTr("Log odds ratio (2x2 only)")
				CIField { name: "oddsRatioConfidenceIntervalInterval"; label: qsTr("Confidence interval") }
			}
			CheckBox { name: "VovkSellkeMPR";	label: qsTr("Vovk-Sellke maximum p-ratio") }
		}

		Group
		{
			title: qsTr("Nominal")
			CheckBox { name: "contingencyCoefficient" ; label: qsTr("Contingency coefficient")				}
			CheckBox { name: "phiAndCramersV";			label: qsTr("Phi and Cramer's V")					}
			CheckBox { name: "lambda";					label: qsTr("Lambda");					debug: true }
			CheckBox { name: "uncertaintyCoefficient";	label: qsTr("Uncertainty coefficient");	debug: true }
		}

		Group
		{
			title: qsTr("Ordinal")
			CheckBox { name: "gamma";			label: qsTr("Gamma")						}
			CheckBox { name: "somersD";			label: qsTr("Somers' d"); debug: true	}
			CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b")			}
			CheckBox { name: "kendallsTauC";	label: qsTr("Kendall's tau-c"); debug: true }
		}

		Group
		{
			debug: true
			title: qsTr("Nominal By Interval")
			CheckBox { name: "byIntervalEta"; label: qsTr("Eta") }
		}
		
		Group
		{
			debug: true
			Layout.columnSpan: 2
			CheckBox
			{
				name: "cochransAndMantel"; label: qsTr("Cochran's and Mantel-Haenszel statistics")
				DoubleField { name: "testOddsRatioEquals"; label: qsTr("Test common odds ratio equals"); defaultValue: 1 }
			}
		}
	}
	
	Section
	{
		title: qsTr("Cells")
		
		Group
		{
			title: qsTr("Counts")
			CheckBox { name: "countsExpected";	label: qsTr("Expected") }
			CheckBox
			{
				name: "hideSmallCounts"; label: qsTr("Hide small counts"); debug: true
				IntegerField { name: "hideSmallCountsLessThan"; label: qsTr("Less than"); defaultValue: 5; debug: true }
			}
		}

		Group
		{
			title: qsTr("Z-Test")
			debug: true
			CheckBox
			{
				name: "zTestCompareColumns"; label: qsTr("Compare column proportions")
				CheckBox { name: "zTestAdjustPValues";	label: qsTr("Adjust p-values") }
			}
		}

		Group
		{
			title: qsTr("Percentages")
			CheckBox { name: "percentagesRow";		label: qsTr("Row")		}
			CheckBox { name: "percentagesColumn";	label: qsTr("Column")	}
			CheckBox { name: "percentagesTotal";	label: qsTr("Total")		}
		}

		Group
		{
			title: qsTr("Residuals")
			debug: true
			CheckBox { name: "residualsUnstandardized";			label: qsTr("Unstandardized")		}
			CheckBox { name: "residualsStandardized";			label: qsTr("Standardized")			}
			CheckBox { name: "residualsAdjustedStandardized";	label: qsTr("Adjusted standardized")	}
		}
	}
	
	Section
	{
		title: qsTr("Options")
		
		RadioButtonGroup
		{
			name: "rowOrder"
			title: qsTr("Row Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true	}
			RadioButton { value: "descending";	label: qsTr("Descending")				}
		}
		RadioButtonGroup
		{
			name: "columnOrder"
			title: qsTr("Column Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true	}
			RadioButton { value: "descending";	label: qsTr("Descending")				}
		}
	}
}
