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
import JASP.Widgets 1.0


Form
{
	usesJaspResults: false
	
	VariablesForm
	{
		height: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "variables"; suggestedColumns: ["ordinal", "scale"] }
	}

	Group
	{
		title: qsTr("Correlation Coefficient")
		CheckBox { name: "pearson";			label: qsTr("Pearson's rho"); checked: true	}
		CheckBox { name: "spearman";		label: qsTr("Spearman"); debug: true		}
		CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b")				}
	}

	Group
	{
		title: qsTr("Additional Options")
		CheckBox { name: "reportBayesFactors";	label: qsTr("Report Bayes factors"); checked: true		}
		CheckBox { name: "flagSupported";		label: qsTr("Flag supported correlations")				}
		CheckBox
		{
			name: "credibleInterval"; label: qsTr("Credible intervals")
			CIField { name: "ciValue";	label: qsTr("Interval"); debug: true }
		}
	}
	
	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Alt. Hypothesis")
		RadioButton { value: "correlated";				label: qsTr("Correlated"); checked: true	}
		RadioButton { value: "correlatedPositively";	label: qsTr("Correlated positively")		}
		RadioButton { value: "correlatedNegatively";	label: qsTr("Correlated negatively")		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotCorrelationMatrix"; label: qsTr("Correlation matrix")
			CheckBox { name: "plotDensitiesForVariables";	label: qsTr("Densities for variables")	}
			CheckBox { name: "plotPosteriors";				label: qsTr("Posteriors under H\u2081")	}
		}
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Prior")
        DoubleField { name: "priorWidth"; label: qsTr("Stretched beta prior width"); defaultValue: 1.0; min: 0.003; max: 2; decimals: 3 }
	}

	Section
	{
		title: qsTr("Options")
		
		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			RadioButton { value: "excludePairwise"; label: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "excludeListwise"; label: qsTr("Exclude cases listwise")				}
		}
	}
}
