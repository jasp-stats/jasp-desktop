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
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0

// All Analysis forms must be built with the From QML item
Form
{
	usesJaspResults: true

	VariablesForm
	{
		height: 400
		AvailableVariablesList { name: "allVariables" }
		AssignedVariablesList { name: "ES_name";	title: qsTr("Effect Size"); singleVariable: true; suggestedColumns: ["scale"] }
		AssignedVariablesList { name: "SE_name";	title: qsTr("Effect Size Variance"); singleVariable: true; suggestedColumns: ["scale"] }
		AssignedVariablesList { name: "fixedEffects";	title: qsTr("Fixed Effects") }
		AssignedVariablesList { name: "inner_grouping";	title: qsTr("Inner Grouping (Multivariate Structure)"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "outer_grouping";	title: qsTr("Outer Grouping (Grouping Structure)"); singleVariable: false; suggestedColumns: ["ordinal", "nominal"] }
	}
	

	//	VariablesForm
	//	{
	//		AssignedVariablesList { name: "variables";	title: qsTr("Variables") }
	//		AssignedVariablesList { name: "splitby";	title: qsTr("Split"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
	//		AssignedVariablesList { name: "test";	title: qsTr("Test"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
	//	}

	DropDown { name: "labels_variable"; label: qsTr("Case labels"); values: allAvailableVariables; }


	Section
	{
		title: qsTr("Fixed Effects Model")
		Group
		{
			title: qsTr("Fixed Effects Model")
			columns: 2
			VariablesForm
			{
				height: 100
				AvailableVariablesList { name: "predictors"; title: qsTr("Predictors"); source: "fixedEffects"}
				AssignedVariablesList  { name: "fixed_predictor_terms"; title: qsTr("Model Terms"); listViewType: "Interaction"}
			}
			CheckBox { name: "intercept"; text: qsTr("Use intercept"); checked: true;}
		}
	}

	Section
	{
		title: qsTr("Random Effects Model")
		Group
		{
			title: qsTr("Random Effects Model")
			columns: 2
			DropDown { name: "inner_cov_struct"; label: qsTr("Multivariate Covariance Model"); values: ["CS","HCS","UN","ID","DIAG","AR","HAR"]}
		}
	}

	Section
	{
		title: qsTr("Statistics")
		Group
		{
			title: qsTr("Omnibus Statistics")
			CheckBox
			{
				name: "anova_table"; text: qsTr("Anova Table"); checked: true;
				//CheckBox { name: "add_robust_qtest"; text: qsTr("Include robust tests");}
			}
			CheckBox { name: "fit_measures_table"; text: qsTr("Fit Indices")}
		}
		Group
		{
			title: qsTr("Coefficients")
			CheckBox { name: "FE_coef_table"; text: qsTr("Fixed Effects Coefficients Table"); checked: true;}
			CheckBox { name: "varComp_params_table"; text: qsTr("Random Effects Coefficients Table")}
		}

		Group
		{
			title: qsTr("Model Derived Statistics")
			CheckBox { name: "emmeans_table"; text: qsTr("Estimated Marginal Means")}
		}
	}

	Section
	{
		title: qsTr("Plots")

		CheckBox { name: "funnel_plot"; text: qsTr("Funnel plot") }
		CheckBox { name: "forest_plot"; text: qsTr("Forest plot"); checked: true; }
	}

	Section
	{
		title: qsTr("Advanced")
		DropDown { name: "method"; label: qsTr("Method"); values: ["ML", "REML"]; }
		CheckBox { name: "FE_vcov_table"; text: qsTr("Fixed Effects Covariance Matrix")}
		//RadioButtonGroup {
		//  title: qsTr("Test of coefficients")
		//  name: "coeftest_type"
		//  RadioButton { value: "z"; label: qsTr("Wald type (asymptotic <em>z</em>-test)"); checked: true }
		//  RadioButton { value: "t"; label: qsTr("<em>t</em>-test (assuming normality)") }
		//}
	}

	Section
	{
		title: qsTr("Diagnostics")
		Group
		{
			CheckBox
			{
				name: "funnelPlotAsymmetry"; text: qsTr("Test for Funnel Plot Asymmetry")
				CheckBox { name: "ranktest_table"; text: qsTr("Rank correlation test")}
				CheckBox { name: "regtest_table"; text: qsTr("Egger's Regression test")}
			}
			CheckBox { name: "failsafe_n_table"; text: qsTr("Fail-safe-N analysis (controversial)")}
		}
		Group
		{
			CheckBox { name: "diagnostics_table"; text: qsTr("Diagnostics Table")}
			CheckBox
			{
				name: "diagnosticPlots"; text: qsTr("Diagnostic plots")
				CheckBox { name: "residual_dependent_plot"; text: qsTr("Residual vs Dependent") }
				CheckBox { name: "profile_plot"; text: qsTr("Profile Random Effects Parameters (takes time)") }
			}
		}
	}


}
