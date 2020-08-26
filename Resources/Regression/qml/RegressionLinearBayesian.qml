// Copyright (C) 2013-2018 University of Amsterdam
//
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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0


Form {

	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";	title: qsTr("Dependent Variable");		suggestedColumns: ["scale"];	singleVariable: true	}
		AssignedVariablesList	{ name: "covariates";	title: qsTr("Covariates");				suggestedColumns: ["scale"];	allowedColumns: ["scale"]}
		AssignedVariablesList	{ name: "wlsWeights";	title: qsTr("WLS Weights (optional)");	suggestedColumns: ["scale"];	singleVariable: true	}
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Output")
		columns: 1

		CheckBox{ name: "postSummaryTable"; label: qsTr("Posterior summary"); id: postSummaryTable
			RadioButtonGroup
			{
				name: "effectsType"
				RadioButton { value: "allModels";		label: qsTr("Across all models");   checked: true	}
				RadioButton { value: "matchedModels";	label: qsTr("Across matched models")				}
			}
		}

		CheckBox
		{
			name: "postSummaryPlot"
			label: qsTr("Plot of coefficients")
			id: postSummaryPlot
			CheckBox { name: "omitIntercept"; label: qsTr("Omit intercept") }

		}

		DropDown
		{
			name: "summaryType"
			enabled: postSummaryTable.checked || postSummaryPlot.checked
			indexDefaultValue: 3
			values: [
				{ label: qsTr("Best model"),			value: "best"		},
				{ label: qsTr("Most complex model"),	value: "complex"	},
				{ label: qsTr("Median model"),			value: "median"		},
				{ label: qsTr("Model averaged"),		value: "averaged"	}
			]
		}



		CIField
		{
			name: "posteriorSummaryPlotCredibleIntervalValue"
			label: qsTr("Credible interval")
			enabled: postSummaryTable.checked || postSummaryPlot.checked
		}

	}

	RadioButtonGroup
	{
		name: "bayesFactorOrder"
		title: qsTr("Order")
		RadioButton { value: "bestModelTop"; label: qsTr("Compare to best model"); checked: true	}
		RadioButton { value: "nullModelTop"; label: qsTr("Compare to null model")					}
	}

	RadioButtonGroup
	{
		name: "shownModels"
		title: qsTr("Limit No. Models Shown")
		RadioButton { value: "unlimited"; label: qsTr("No") }
		RadioButton { 
			value: "limited"
			label: qsTr("Yes, show best")
			checked: true
			childrenOnSameRow: true
			IntegerField { name: "numShownModels"; defaultValue: 10; min: 1}
		}

	}

	Group
	{
		title: qsTr("Data")
		CheckBox { name: "descriptives"; label: qsTr("Descriptives") }
	}
	
	Section
	{
		title: qsTr("Model")

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			
			AvailableVariablesList
			{
				name: "availableTerms"
				title: qsTr("Components")
				width: parent.width / 4
				source: ['covariates']
			}

			ModelTermsList { width: parent.width * 5 / 9 }

		}

	}
	
	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Coefficients")
			CheckBox { name: "plotInclusionProbabilities";	label: qsTr("Inclusion probabilities")			}
			CheckBox { name: "plotCoefficientsPosterior";	label: qsTr("Marginal posterior distributions")	}
		}

		Group
		{
			title: qsTr("Models")
			CheckBox { name: "plotLogPosteriorOdds";	label: qsTr("Log posterior odds")				}
			CheckBox { name: "plotModelComplexity";		label: qsTr("Log(P(data|M)) vs. model size")	}
			CheckBox { name: "plotModelProbabilities";	label: qsTr("Model probabilities")				}
		}

		Group
		{
			title: qsTr("Residuals")
			CheckBox { name: "plotResidualsVsFitted";	label: qsTr("Residuals vs. fitted")					}
			CheckBox { name: "plotQQplot";				label: qsTr("Q-Q plot of model averaged residuals")	}
		}
	}
	
	Section
	{
		title: qsTr("Advanced Options")

		RadioButtonGroup
		{
			name: "priorRegressionCoefficients"
			title: qsTr("Prior")

			RadioButton { value: "AIC";			label: qsTr("AIC")		}
			RadioButton { value: "BIC";			label: qsTr("BIC")		}
			RadioButton { value: "EB-global";	label: qsTr("EB-global")}
			RadioButton { value: "EB-local";	label: qsTr("EB-local")	}
			RadioButton { value: "g-prior";		label: qsTr("g-prior")	}
			GridLayout
			{
				rowSpacing: jaspTheme.rowGroupSpacing
				columnSpacing: 0
				Group
				{
					RadioButton { value: "hyper-g";			label: qsTr("Hyper-g");				id: hyperg			}
					RadioButton { value: "hyper-g-laplace";	label: qsTr("Hyper-g-Laplace");		id: hyperglaplace	}
					RadioButton { value: "hyper-g-n";		label: qsTr("Hyper-g-n");			id: hypergn			}
				}
				DoubleField
				{
					name: "alpha"
					label: qsTr("alpha")
					enabled: hyperg.checked || hyperglaplace.checked || hypergn.checked
					defaultValue: 3.0
					min: 2
					max: 4
					inclusive: JASP.None
				}
				RadioButton { value: "JZS"; label: qsTr("JZS"); checked: true; id: jzs }
				DoubleField
				{
					name: "rScale"
					label: qsTr("r scale")
					enabled: jzs.checked
					fieldWidth: 50
					defaultValue: 0.354
					max: 100000
					inclusive: JASP.MaxOnly
				}
			}
		}

		ColumnLayout
		{
			RadioButtonGroup
			{
				name: "modelPrior"
				title: qsTr("Model Prior")
				RadioButton
				{
					checked: true
					value: "beta.binomial"; label: qsTr("Beta binomial")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "betaBinomialParamA"; label: qsTr("a"); defaultValue: 1; inclusive: JASP.MaxOnly}
					DoubleField { name: "betaBinomialParamB"; label: qsTr("b"); defaultValue: 1; inclusive: JASP.MaxOnly}
				}
				RadioButton { value: "uniform"; label: qsTr("Uniform")}
				RadioButton
				{
					value: "Wilson"
					label: qsTr("Wilson")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "wilsonParamLambda"; label: qsTr("λ"); defaultValue: 1; inclusive: JASP.None; min: 0}
				}
				RadioButton
				{
					value: "Castillo"
					label: qsTr("Castillo")
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "castilloParamU"; label: qsTr("u"); defaultValue: 1; inclusive: JASP.MinMax; min: 1}
				}
				RadioButton
				{
					value: "Bernoulli"; label: qsTr("Bernoulli")
					childrenOnSameRow: true
					DoubleField { name: "bernoulliParam"; label: qsTr("p"); defaultValue: 0.5; max: 1; inclusive: JASP.None; decimals: 3 }
				}
			}

			RadioButtonGroup
			{
				name: "samplingMethod"
				title: qsTr("Sampling Method")
				RadioButton
				{
					value: "BAS"; label: qsTr("BAS"); checked: true
					childrenOnSameRow: true
					IntegerField { name: "numberOfModels"; label: qsTr("No. models"); defaultValue: 0; max: 100000000 }
				}
				RadioButton
				{
					value: "MCMC"; label: qsTr("MCMC")
					childrenOnSameRow: true
					IntegerField { name: "iterationsMCMC"; label: qsTr("No. samples"); defaultValue: 0; max: 100000000 }
				}
			}

			Group
			{
				title: qsTr("Numerical Accuracy")
				IntegerField
				{
					name: "nSimForCRI"
					label: qsTr("No. samples for credible interval")
					defaultValue: 1000
					fieldWidth: 50
					min: 100
					max: 1000000
				}
			}

			SetSeed{}

		}
	}

}
