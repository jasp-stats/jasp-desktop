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
		AssignedVariablesList { name: "variables"; allowedColumns: ["ordinal", "scale"] }
	}
	
	Group
	{
		title: qsTr("Correlation Coefficients")
		CheckBox { name: "pearson";			text: qsTr("Pearson"); checked: true	}
		CheckBox { name: "spearman";		text: qsTr("Spearman")					}
		CheckBox { name: "kendallsTauB";	text: qsTr("Kendall's tau-b")			}
	}

	Group
	{
		CheckBox { name: "displayPairwise";		text: qsTr("Display pairwise table")				}
		CheckBox { name: "reportSignificance";	text: qsTr("Report significance"); checked: true	}
		CheckBox { name: "flagSignificant";		text: qsTr("Flag significant correlations")			}
		CheckBox
		{
			name: "confidenceIntervals";		text: qsTr("Confidence intervals")
			PercentField { name: "confidenceIntervalsInterval"; text: qsTr("Interval"); defaultValue: 95 }
		}
		CheckBox { name: "VovkSellkeMPR";		text: qsTr("Vovk-Sellke maximum p-ratio")			}
	}
	
	RadioButtonGroup
	{
		title: qsTr("Hypothesis")
		name: "hypothesis"
		RadioButton { value: "correlated";				text: qsTr("Correlated"); checked: true	}
		RadioButton { value: "correlatedPositively";	text: qsTr("Correlated positively")		}
		RadioButton { value: "correlatedNegatively";	text: qsTr("Correlated negatively")		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotCorrelationMatrix"; text: qsTr("Display pairwise table")
			CheckBox { name: "plotDensities";		text: qsTr("Densities for variables")	}
			CheckBox { name: "plotStatistics";		text: qsTr("Statistics")				}
		}

	}
	
	ExpanderButton
	{
		title: qsTr("Options")
		debug: true
		
		Group
		{
			title: qsTr("Statistics")
			CheckBox { name: "meansAndStdDev";	text: qsTr("Means and standard deviations")				}
			CheckBox { name: "crossProducts";	text: qsTr("Cross-product deviations and covariances")	}
		}
		
		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			RadioButton { value: "excludePairwise"; text: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "excludeListwise"; text: qsTr("Exclude cases listwise")				}
		}
	}
}
