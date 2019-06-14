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
		height: 400
		AvailableVariablesList { name: "allVariables" }
		AssignedVariablesList { name: "dependent";	title: qsTr("Effect Size"); singleVariable: true; suggestedColumns: ["scale"] }
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("Effect Size Standard Error"); singleVariable: true; suggestedColumns: ["scale"] }
		DropDown { name: "method"; label: qsTr("Method"); currentIndex: 2; values: [ "Fixed Effects", "Maximum Likelihood", "Restricted ML", "DerSimonian-Laird", "Hedges", "Hunter-Schmidt", "Sidik-Jonkman", "Empirical Bayes", "Paule-Mandel"]; }
        AssignedVariablesList { name: "studyLabels";	title: qsTr("Study Labels"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates"); singleVariable: false; suggestedColumns: ["scale"] }
		AssignedVariablesList { name: "factors";	title: qsTr("Factors"); singleVariable: false; suggestedColumns: ["ordinal", "nominal"] }
	}

	Section
	{
		title: qsTr("Model")
		VariablesForm
		{
			height: 150
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["covariates","factors"]}
			AssignedVariablesList  { name: "modelTerms"; title: qsTr("Model Terms"); listViewType: "Interaction"}
		}

		CheckBox { name: "includeConstant"; text: qsTr("Include intercept"); checked: true }
	}

	Section
	{
		title: qsTr("Statistics")
		Group
		{
			title: qsTr("Regression Coefficients")
			CheckBox { name: "regressionCoefficientsEstimates"; text: qsTr("Estimates"); checked: true }
			CheckBox
			{
				name: "regressionCoefficientsConfidenceIntervals"; text: qsTr("Confidence intervals")
				CIField { name: "regressionCoefficientsConfidenceIntervalsInterval"; label: qsTr("Interval") }
				DropDown { name: "test"; label: qsTr("Test"); values: [ "z", "knha"]; }
			}
			CheckBox { name: "regressionCoefficientsCovarianceMatrix"; text: qsTr("Covariance matrix") }

		}
		Group
		{
			title: qsTr("Model Fit")
			CheckBox { name: "modelFit";				text: qsTr("Fit measures") }
			CheckBox { name: "forestPlot";				text: qsTr("Forest plot") }
			CheckBox { name: "funnelPlot";				text: qsTr("Funnel plot") }
			CheckBox { name: "rSquaredChange";			text: qsTr("Rank test for funnel plot asymmetry") }
			CheckBox { name: "funnelPlotAsymmetryTest"; text: qsTr("Regression test for funnel plot asymmetry") }
		}

		Group
		{
			title: qsTr("Residuals Model")
			CheckBox { name: "residualsParameters"; text: qsTr("Residuals parameters"); checked: true;}
		}
	}

	Section
	{
		title: qsTr("Diagnostics")
		Group
		{
			title: qsTr("Plots")
			CheckBox { name: "trimfillPlot";			text: qsTr("Trim-fill analysis")	}
			CheckBox { name: "plotResidualsPredicted";	text: qsTr("Profile plot")			}
			CheckBox
			{
				name: "plotResidualsDependent"; text: qsTr("Diagnostic plots")
				CheckBox { name: "plotResidualsQQ"; text: qsTr("Q-Q plot standardized residuals"); checked: true }
			}
		}
		Group
		{
			title: qsTr("Robustness")
			CheckBox { name: "plotResidualsCovariates";			text: qsTr("Fail-safe N")			}
			CheckBox { name: "residualsCasewiseDiagnostics";	text: qsTr("Casewise diagnostics")	}
		}
	}
}
