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
import JASP.Theme 1.0

Form
{
	usesJaspResults: false

	VariablesForm
	{
		height: Theme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); suggestedColumns: ["ordinal", "scale"] }
	}

	Group
	{
		title: qsTr("Correlation Coefficient")
		CheckBox { name: "pearson";			label: qsTr("Pearson"); checked: true	}
		CheckBox { name: "spearman";		label: qsTr("Spearman")					}
		CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b")			}
	}

	Group
	{
		title: qsTr("Additional Options")
		CheckBox { name: "displayPairwise";		label: qsTr("Display pairwise table")				}
		CheckBox { name: "reportSignificance";	label: qsTr("Report significance"); checked: true	}
		CheckBox { name: "flagSignificant";		label: qsTr("Flag significant correlations")			}
		CheckBox
		{
			name: "confidenceIntervals";		label: qsTr("Confidence intervals")
			CIField { name: "confidenceIntervalsInterval"; label: qsTr("Interval") }
		}
		CheckBox { name: "VovkSellkeMPR";		label: qsTr("Vovk-Sellke maximum p-ratio")			}
	}

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "hypothesis"
		RadioButton { value: "correlated";				label: qsTr("Correlated"); checked: true	}
		RadioButton { value: "correlatedPositively";	label: qsTr("Correlated positively")		}
		RadioButton { value: "correlatedNegatively";	label: qsTr("Correlated negatively")		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotCorrelationMatrix"; label: qsTr("Correlation Matrix")
			CheckBox { name: "plotDensities";		label: qsTr("Densities for variables")	}
			CheckBox { name: "plotStatistics";		label: qsTr("Statistics")				}
		}

	}

	Section
	{
		title: qsTr("Options")
		debug: true

		Group
		{
			title: qsTr("Statistics")
			CheckBox { name: "meansAndStdDev";	label: qsTr("Means and standard deviations")				}
			CheckBox { name: "crossProducts";	label: qsTr("Cross-product deviations and covariances")		}
		}

		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			RadioButton { value: "excludePairwise"; label: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "excludeListwise"; label: qsTr("Exclude cases listwise")					}
		}
	}
}
