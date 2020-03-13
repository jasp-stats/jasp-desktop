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

    VariablesForm
    {
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "variables"; suggestedColumns: ["ordinal", "scale"] }
    }

    Group
    {
        title: qsTr("Correlation Coefficient")
        CheckBox { name: "pearson";         label: qsTr("Pearson"); checked: true       }
        CheckBox { name: "spearman";        label: qsTr("Spearman"); debug: true		}
        CheckBox { name: "kendall";         label: qsTr("Kendall's tau-b")				}
    }

    Group
    {
        title: qsTr("Additional Options")
        CheckBox { name: "displayPairwise";		label: qsTr("Display pairwise table")                   }
        CheckBox { name: "reportBayesFactors";	label: qsTr("Report Bayes factors"); checked: true		}
        CheckBox { name: "flagSupported";		label: qsTr("Flag supported correlations")				}
        CheckBox { name: "reportN";             label: qsTr("Sample size")                              }
        CheckBox { name: "posteriorMedian";     label: qsTr("Posterior median"); debug: true			}
        CheckBox
        {
            name: "ci"; label: qsTr("Credible intervals")
            CIField { name: "ciValue";	label: qsTr("Interval") }
        }
    }

    RadioButtonGroup
    {
        name: "alternative"
        title: qsTr("Alt. Hypothesis")
        RadioButton { value: "two.sided";		label: qsTr("Correlated"); checked: true	}
        RadioButton { value: "greater";         label: qsTr("Correlated positively")		}
        RadioButton { value: "less";            label: qsTr("Correlated negatively")		}
    }

    Group
    {
        title: qsTr("Plots")
        CheckBox
        {
            name: "plotMatrix"; label: qsTr("Correlation matrix")
            CheckBox { name: "plotMatrixDensities";                 label: qsTr("Densities for variables")	}
            CheckBox { name: "plotMatrixPosteriors";				label: qsTr("Posteriors under H\u2081")	}
        }
    }

    BayesFactorType {}

    Group
    {
        title: qsTr("Prior")
        DoubleField { name: "kappa"; label: qsTr("Stretched beta prior width"); defaultValue: 1.0; min: 0.003; max: 2; decimals: 3 }
    }

    Section
    {
        title: qsTr("Plot Individual Pairs")
        VariablesForm
        {
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
            AvailableVariablesList      { name: "allVariablesList2";     source:"variables" }
            AssignedPairsVariablesList  { name:  "pairs";                suggestedColumns: ["ordinal", "scale"] }
        }

        RadioButtonGroup
        {
            name: "pairsMethod"
            title: qsTr("Correlation coefficient to plot")
            RadioButton { value: "pearson";     label: qsTr("Pearson");         checked: true	}
            RadioButton { value: "spearman";    label: qsTr("Spearman's rho");  debug:   true   }
            RadioButton { value: "kendall";     label: qsTr("Kendall's tau")                    }
        }

        CheckBox
        {
            name: "plotScatter";                    label: qsTr("Scatterplot"); checked: true
            CheckBox { name: "plotScatterAddInfo";  label: qsTr("Robustness check"); debug: true }

        }
        CheckBox
        {
            name: "plotPriorPosterior";			label: qsTr("Prior and posterior")
            CheckBox { name: "plotPriorPosteriorAddEstimationInfo";	label: qsTr("Estimation info"); checked: true }
            CheckBox { name: "plotPriorPosteriorAddTestingInfo";	label: qsTr("Testing info"); checked: true }
        }
        CheckBox
        {
            name: "plotBfRobustness";		label: qsTr("Bayes factor robustness check")
            CheckBox { name: "plotBfRobustnessAddInfo"; label: qsTr("Additional info"); checked: true }
        }
        CheckBox
        {
            name: "plotBfSequential";                   label: qsTr("Sequential analysis")
            CheckBox { name: "plotBfSequentialAddInfo"; label: qsTr("Additional info"); checked: true}
        }
    }

    Section
    {
        title: qsTr("Options")

        RadioButtonGroup
        {
            name: "missingValues"
            title: qsTr("Missing Values")
            RadioButton { value: "excludePairwise"; label: qsTr("Exclude cases pairwise"); checked: true	}
            RadioButton { value: "excludeListwise"; label: qsTr("Exclude cases listwise")                   }
        }

		SetSeed{}
    }
}
