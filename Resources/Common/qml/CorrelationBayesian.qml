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
import JASP.Theme 1.0

Form
{
	usesJaspResults: false
	
	VariablesForm
	{
		AssignedVariablesList { name: "variables"; allowedColumns: ["ordinal", "scale"] }
	}
	
	GroupBox
	{
		title: qsTr("Correlation Coefficients")
		spacing: Theme.rowGridSpacing
		
		GridLayout
		{
			GroupBox
			{
				CheckBox { name: "pearson";			text: qsTr("Pearson's rho"); checked: true	}
				CheckBox { name: "spearman";		text: qsTr("Spearman"); debug: true			}
				CheckBox { name: "kendallsTauB";	text: qsTr("Kendall's tau-b")				}
			}
			
			GroupBox
			{
				CheckBox { name: "reportBayesFactors";	text: qsTr("Report Bayes factors"); checked: true		}
				CheckBox { name: "flagSupported";		text: qsTr("Flag supported correlations")				}
				CheckBox { name: "credibleInterval";	text: qsTr("Credible intervals"); id: credibleInterval	}
				PercentField { name: "ciValue";			text: qsTr("Interval"); defaultValue: 95; enabled: credibleInterval.checked; indent: true }
			}
		}
	}
	
	GridLayout
	{
		RadioButtonGroup
		{
			name: "hypothesis"
			title: qsTr("Hypothesis")
			RadioButton { value: "correlated";				text: qsTr("Correlated"); checked: true	}
			RadioButton { value: "correlatedPositively";	text: qsTr("Correlated positively")		}
			RadioButton { value: "correlatedNegatively";	text: qsTr("Correlated negatively")		}
		}
		
		GroupBox
		{
			title: qsTr("Plots")
			CheckBox { name: "plotCorrelationMatrix"; text: qsTr("Correlation matrix"); id: plotCorrelationMatrix }
			GroupBox
			{
				enabled: plotCorrelationMatrix.checked
				indent: true
				CheckBox { name: "plotDensitiesForVariables";	text: qsTr("Densities for variables")	}
				CheckBox { name: "plotPosteriors";				text: qsTr("Posteriors under H\u2081")	}
			}
		}
		
		BayesFactorType {}
		
		DoubleField { name: "priorWidth"; text: qsTr("Stretched beta prior width"); defaultValue: 1.0; doubleValidator { top: 2; decimals: 1 } }
	}
	
	ExpanderButton
	{
		title: qsTr("Options")
		
		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			RadioButton { value: "excludePairwise"; text: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "excludeListwise"; text: qsTr("Exclude cases listwise")				}
		}
	}
}
