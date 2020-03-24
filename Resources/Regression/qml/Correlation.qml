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

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList{  name: "allVariablesList" }
		AssignedVariablesList {  name: "variables";				title: qsTr("Variables");	 suggestedColumns: ["ordinal", "scale"] }
		AssignedVariablesList {  name: "conditioningVariables"; title: qsTr("Condition on"); suggestedColumns: ["ordinal", "scale"]; debug: true }
	}

	Group
	{
        title: qsTr("Sample Correlation Coefficient")
        CheckBox { name: "pearson";			label: qsTr("Pearson's r"); checked: true	}
        CheckBox { name: "spearman";		label: qsTr("Spearman's rho")					}
		CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b")			}
	}

	Group
	{
		title: qsTr("Additional Options")
		CheckBox { name: "displayPairwise";		label: qsTr("Display pairwise")									}
		CheckBox { name: "reportSignificance";	label: qsTr("Report significance");				checked: true	}
		CheckBox { name: "flagSignificant";		label: qsTr("Flag significant correlations")					}
		CheckBox
		{
			name: "confidenceIntervals";		label: qsTr("Confidence intervals")
			CIField { name: "confidenceIntervalsInterval"; label: qsTr("Interval") }
		}
		CheckBox { name: "VovkSellkeMPR";		label: qsTr("Vovk-Sellke maximum p-ratio")			}
		CheckBox { name: "sampleSize";			label: qsTr("Sample size") }
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
			name: "plotCorrelationMatrix";			label: qsTr("Scatter plots")
			CheckBox { name: "plotDensities";		label: qsTr("Densities for variables")	}
			CheckBox { name: "plotStatistics";		label: qsTr("Statistics")				}
		}
		CheckBox{ name: "plotHeatmap"; label: qsTr("Heatmap") }

	}

	Section
	{
		title: qsTr("Assumption Checks")

		Group
		{
			title: qsTr("Multivariate Normality")
			CheckBox { name: "multivariateShapiro"; label: qsTr("Shapiro")			   }
			CheckBox { name: "multivariateRoyston"; label: qsTr("Royston"); debug: true  }
			CheckBox { name: "multivariateMardia" ; label: qsTr("Mardia");  debug: true  }
			CheckBox { name: "multivariateEnergy" ; label: qsTr("Energy");  debug: true  }
		}

		Group
		{
			title: qsTr("Pairwise Normality")
			CheckBox { name: "pairwiseShapiro"; label: qsTr("Shapiro")			   }
			CheckBox { name: "pairwiseRoyston"; label: qsTr("Royston"); debug: true  }
			CheckBox { name: "pairwiseMardia" ; label: qsTr("Mardia");  debug: true  }
			CheckBox { name: "pairwiseEnergy" ; label: qsTr("Energy");  debug: true  }
		}
	}

	Section
	{
		title: qsTr("Options")

		Group
		{
			title: qsTr("Statistics")
			debug: true
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
