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

Form {
	id: form
	
	VariablesForm
	{
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	allowedColumns: ["scale"]; singleItem: true	}	
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			allowedColumns: ["scale"]					}
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)"); allowedColumns: ["scale"]; singleItem: true }
	}
	
	GridLayout 
	{
		BayesFactorType {}
		
		GroupBox
		{
			title: qsTr("Output")
			GridLayout
			{
				rowSpacing: Theme.rowGroupSpacing                
				CheckBox { text: qsTr("Posterior summary"); name: "postSummaryTable"; id: postSummaryTable }
				ComboBox
				{
					enabled: postSummaryTable.checked
					name: "summaryType"
					currentIndex: 3
					model: ListModel
					{
						ListElement { key: "Best model"; value: "best" }
						ListElement { key: "Most complex model"; value: "complex" }
						ListElement { key: "Median model"; value: "median" }
						ListElement { key: "Model averaged"; value: "averaged" }
					}
				}
				
				CheckBox { text: qsTr("Plot of coefficients")   ; name: "postSummaryPlot" ; id: postSummaryPlot}
				
				GroupBox
				{
					enabled: postSummaryPlot.checked
					CheckBox    { text: qsTr("Omit intercept")          ; name: "omitIntercept" ;  }
					PercentField { text: qsTr("Credible interval")      ; name: "posteriorSummaryPlotCredibleIntervalValue" ;  defaultValue: 95 }
				}
			}
		}
		
		RadioButtonGroup
		{
			title: qsTr("Order")
			name: "bayesFactorOrder"
			RadioButton { text: qsTr("Compare to best model"); name: "bestModelTop"; checked: true }
			RadioButton { text: qsTr("Compare to null model"); name: "nullModelTop" }
		}
		
		RadioButtonGroup
		{
			title: qsTr("Limit no. models shown")
			name: "shownModels"
			RadioButton { text: qsTr("No"); name: "limited" }
			RowLayout
			{
				RadioButton { text: qsTr("Yes, show best"); name: "unlimited"; checked: true }
				IntegerField { name: "numShownModels"; defaultValue: 10; intValidator.bottom: 1 }
			}
		}
		
		GroupBox
		{
			title: qsTr("Data")
			CheckBox { text: qsTr("Descriptives"); name: "descriptives" }
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			
			availableVariablesList
			{
				name: "availableTerms"
				title: qsTr("Components")
				width: parent.width / 4
				syncModels: ['covariates']
			}
			AssignedVariablesList
			{
				name: "modelTerms"
				title: qsTr("Model terms")
				listViewType: "Interaction"
				ExtraControlColumn {
					type: "CheckBox"
					name: "isNuisance"
					title: qsTr("Add to null model")
				}
			}
		}
		
	}
	
	ExpanderButton
	{
		text: qsTr("Plots")
		
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Coefficients")
				CheckBox { text: qsTr("Inclusion probabilities"); name: "plotInclusionProbabilities" }
				CheckBox { text: qsTr("Marginal posterior distributions"); name: "plotCoefficientsPosterior" }
			}
			
			GroupBox
			{
				title: qsTr("Models")
				CheckBox { text: qsTr("Log posterior odds"); name: "plotLogPosteriorOdds" }
				CheckBox { text: qsTr("Log(P(data)M)) vs. model size"); name: "plotModelComplexity" }
				CheckBox { text: qsTr("Model probabilities"); name: "plotModelProbabilities" }
			}
			
			GroupBox
			{
				title: qsTr("Residuals")
				CheckBox { text: qsTr("Residuals vs. fitted"); name: "plotResidualsVsFitted" }
			}
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Advanced Options")
		
		GridLayout
		{
			RadioButtonGroup
			{
				title: qsTr("Prior")
				name: "priorRegressionCoefficients"
				
				RadioButton { text: qsTr("AIC"); name: "AIC" }
				RadioButton { text: qsTr("BIC"); name: "BIC" }
				RadioButton { text: qsTr("EB-global"); name: "EB-global" }
				RadioButton { text: qsTr("EB-local"); name: "EB-local" }
				RadioButton { text: qsTr("g-prior"); name: "g-prior" }
				GridLayout
				{
					rowSpacing: Theme.rowGroupSpacing                    
					GroupBox
					{
						RadioButton { text: qsTr("Hyper-g"); name: "hyper-g"; id: hyperg }
						RadioButton { text: qsTr("Hyper-g-Laplace"); name: "hyper-g-laplace"; id: hyperglaplace }
						RadioButton { text: qsTr("Hyper-g-n"); name: "hyper-g-n"; id: hypergn }
					}
					IntegerField
					{
						text: qsTr("alpha");
						name: "alpha";
						enabled: hyperg.checked || hyperglaplace.checked || hypergn.checked
						defaultValue: 3
					}
					RadioButton { text: qsTr("JZS"); name: "JZS"; checked: true; id: jzs }
					DoubleField
					{
						text: qsTr("r scale")
						name: "rScale"
						enabled: jzs.checked
						fieldWidth: 50
						defaultValue: 0.354
						doubleValidator.top: 100000
					}
				}
			}
			
			ColumnLayout
			{
				RadioButtonGroup
				{
					title: qsTr("Model prior")
					name: "modelPrior"
					GridLayout
					{
						rowSpacing: Theme.rowGroupSpacing
						RadioButton { text: qsTr("Beta binomial"); name: "beta.binomial"; checked: true; id: betabinomial}
						RowLayout
						{
							enabled: betabinomial.checked
							DoubleField { text: qsTr("a"); name: "betaBinomialParamA"; defaultValue: 1 }
							DoubleField { text: qsTr("b"); name: "betaBinomialParamB"; defaultValue: 1 }
						}
						RadioButton { text: qsTr("Bernouilli"); name: "Bernoulli"; id: bernoulli }
						DoubleField { text: qsTr("p"); name: "bernoulliParam"; defaultValue: 0.5; enabled: bernoulli.checked; doubleValidator { top: 1; decimals: 2 } }
					}
					RadioButton { text: qsTr("Uniform"); name: "uniform" }
				}
				
				RadioButtonGroup
				{
					title: qsTr("Sampling method")
					name: "samplingMethod"
					GridLayout
					{
						rowSpacing: Theme.rowGroupSpacing
						RadioButton { text: qsTr("BAS"); name: "BAS"; checked: true; id: bas }
						IntegerField { text: qsTr("No. models"); defaultValue: 0; name: "numberOfModels"; enabled: bas.checked; intValidator.top: 100000000 }
						RadioButton { text: qsTr("MCMC"); name: "MCMC"; id: mcmc }
						IntegerField { text: qsTr("No. samples"); defaultValue: 0; name: "iterationsMCMC"; enabled: mcmc.checked; intValidator.top: 100000000 }
					}
				}
				
				GroupBox
				{
					title: qsTr("Numerical accuracy")
					IntegerField { text: qsTr("No. samples for credible interval"); defaultValue: 1000; name: "nSimForCRI"; intValidator { bottom: 100; top: 1000000 } }
				}
			}
		}
	}
	
}
