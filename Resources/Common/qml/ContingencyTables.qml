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
		AssignedVariablesList { name: "rows";		title: qsTr("Rows");	allowedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "columns";	title: qsTr("Columns");	allowedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "counts";		title: qsTr("Counts");	allowedColumns: ["scale"]; singleItem: true }
		AssignedVariablesList { name: "layers";		title: qsTr("Layers");	allowedColumns: ["ordinal", "nominal"]; listViewType: "Layers"; height: 120 }
	}
	
	ExpanderButton {
		title: qsTr("Statistics")
		
		GridLayout {
			GroupBox {
				CheckBox { name: "chiSquared";						text: qsTr("χ²"); checked: true			}
				CheckBox { name: "chiSquaredContinuityCorrection";	text: qsTr("χ² continuity correction")	}
				CheckBox { name: "likelihoodRatio";					text: qsTr("Likelihood ratio")			}
			}
			
			GroupBox {
				CheckBox { name: "oddsRatio"; text: qsTr("Log odds ratio (2x2 only)"); id: oddsRatio }
				PercentField { name: "oddsRatioConfidenceIntervalInterval"; text: qsTr("Confidence interval"); enabled: oddsRatio.checked; defaultValue: 95; indent: true }
				CheckBox { name: "VovkSellkeMPR";	text: qsTr("Vovk-Sellke maximum p-ratio") }
			}
			
			GroupBox {
				title: qsTr("Nominal")
				CheckBox { name: "contingencyCoefficient" ; text: qsTr("Contingency coefficient")				}
				CheckBox { name: "phiAndCramersV";			text: qsTr("Phi and Cramer's V")					}
				CheckBox { name: "lambda";					text: qsTr("Lambda");					debug: true }
				CheckBox { name: "uncertaintyCoefficient";	text: qsTr("Uncertainty coefficient");	debug: true }
			}
			
			GroupBox {
				title: qsTr("Ordinal")
				CheckBox { name: "gamma";			text: qsTr("Gamma")						}
				CheckBox { name: "somersD";			text: qsTr("Somers' d"); debug: true	}
				CheckBox { name: "kendallsTauB";	text: qsTr("Kendall's tau-b")			}
				CheckBox { name: "kendallsTauC";	text: qsTr("Kendall's tau-c"); debug: true }
			}
		}
		
		GroupBox {
			debug: true
			title: qsTr("Nominal by interval")
			CheckBox { name: "byIntervalEta"; text: qsTr("Eta") }
		}
		
		GroupBox {
			debug: true
			CheckBox { name: "cochransAndMantel"; text: qsTr("Cochran's and Mantel-Haenszel statistics"); id: cochransAndMantel }
			IntegerField {
				name: "testOddsRatioEquals"
				text: qsTr("Test common odds ratio equals")
				defaultValue: 1
				enabled: cochransAndMantel.checked
				indent: true
			}
		}
	}
	
	ExpanderButton {
		title: qsTr("Cells")
		
		GridLayout {
			GroupBox {
				title: qsTr("Counts")
				CheckBox { name: "countsExpected";	text: qsTr("Expected")												}
				CheckBox { name: "hideSmallCounts"; text: qsTr("Hide small counts"); id: hideSmallCounts; debug: true	}
				IntegerField {
					name: "hideSmallCountsLessThan"
					text: qsTr("Less than")
					defaultValue: 5
					enabled: hideSmallCounts.checked
					indent: true
					debug: true
				}
			}
			
			GroupBox {
				title: qsTr("Z-Test")
				debug: true
				CheckBox { name: "zTestCompareColumns"; text: qsTr("Compare column proportions"); id: zTestCompareColumns }
				CheckBox { name: "zTestAdjustPValues";	text: qsTr("Adjust p-values"); enabled: zTestCompareColumns.checked; indent: true }
			}
			
			GroupBox {
				title: qsTr("Percentages")
				CheckBox { name: "percentagesRow";		text: qsTr("Row")		}
				CheckBox { name: "percentagesColumn";	text: qsTr("Column")	}
				CheckBox { name: "percentagesTotal";	text: qsTr("Total")		}
			}
			
			GroupBox {
				title: qsTr("Residuals")
				debug: true
				CheckBox { name: "residualsUnstandardized"; text: qsTr("Unstandardized")				}
				CheckBox { name: "residualsStandardized"; text: qsTr("Standardized")					}
				CheckBox { name: "residualsAdjustedStandardized"; text: qsTr("Adjusted Standardized")	}
			}
		}
	}
	
	ExpanderButton {
		title: qsTr("Options")
		
		GridLayout {
			RadioButtonGroup {
				name: "rowOrder"
				title: qsTr("Row Order")
				RadioButton { value: "ascending";	text: qsTr("Ascending"); checked: true	}
				RadioButton { value: "descending";	text: qsTr("Descending")				}
			}
			RadioButtonGroup {
				name: "columnOrder"
				title: qsTr("Column Order")
				RadioButton { value: "ascending";	text: qsTr("Ascending"); checked: true	}
				RadioButton { value: "descending";	text: qsTr("Descending")				}
			}
		}
	}
}
