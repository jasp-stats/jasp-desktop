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
	usesJaspResults: false
	
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
				CheckBox { name: "postSummaryTable"; text: qsTr("Posterior summary"); id: postSummaryTable }
				DropDown
				{
					name: "summaryType"
					enabled: postSummaryTable.checked
					indexDefaultValue: 3
					model: ListModel
					{
						ListElement { title: "Best model";			value: "best"		}
						ListElement { title: "Most complex model";	value: "complex"	}
						ListElement { title: "Median model";		value: "median"		}
						ListElement { title: "Model averaged";		value: "averaged"	}
					}
				}
				
				CheckBox { name: "postSummaryPlot"; text: qsTr("Plot of coefficients"); id: postSummaryPlot }
				
				GroupBox
				{
					enabled: postSummaryPlot.checked
					CheckBox { name: "omitIntercept"; text: qsTr("Omit intercept") }
					PercentField { name: "posteriorSummaryPlotCredibleIntervalValue"; text: qsTr("Credible interval"); defaultValue: 95 }
				}
			}
		}
		
		RadioButtonGroup
		{
			name: "bayesFactorOrder"
			title: qsTr("Order")
			RadioButton { value: "bestModelTop"; text: qsTr("Compare to best model"); checked: true	}
			RadioButton { value: "nullModelTop"; text: qsTr("Compare to null model")				}
		}
		
		RadioButtonGroup
		{
			name: "shownModels"
			title: qsTr("Limit no. models shown")
			RadioButton { value: "limited"; text: qsTr("No") }
			RowLayout
			{
				RadioButton { value: "unlimited"; text: qsTr("Yes, show best"); checked: true }
				IntegerField { name: "numShownModels"; defaultValue: 10; intValidator.bottom: 1 }
			}
		}
		
		GroupBox
		{
			title: qsTr("Data")
			CheckBox { name: "descriptives"; text: qsTr("Descriptives") }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			
			availableVariablesList
			{
				name: "availableTerms"
				title: qsTr("Components")
				width: parent.width / 4
				source: ['covariates']
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
		title: qsTr("Plots")
		
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Coefficients")
				CheckBox { name: "plotInclusionProbabilities";	text: qsTr("Inclusion probabilities")			}
				CheckBox { name: "plotCoefficientsPosterior";	text: qsTr("Marginal posterior distributions")	}
			}
			
			GroupBox
			{
				title: qsTr("Models")
				CheckBox { name: "plotLogPosteriorOdds";	text: qsTr("Log posterior odds")				}
				CheckBox { name: "plotModelComplexity";		text: qsTr("Log(P(data)M)) vs. model size")		}
				CheckBox { name: "plotModelProbabilities";	text: qsTr("Model probabilities")				}
			}
			
			GroupBox
			{
				title: qsTr("Residuals")
				CheckBox { name: "plotResidualsVsFitted";	text: qsTr("Residuals vs. fitted")				}
			}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Advanced Options")
		
		GridLayout
		{
			RadioButtonGroup
			{
				name: "priorRegressionCoefficients"
				title: qsTr("Prior")
				
				RadioButton { value: "AIC";			text: qsTr("AIC")		}
				RadioButton { value: "BIC";			text: qsTr("BIC")		}
				RadioButton { value: "EB-global";	text: qsTr("EB-global")	}
				RadioButton { value: "EB-local";	text: qsTr("EB-local")	}
				RadioButton { value: "g-prior";		text: qsTr("g-prior")	}
				GridLayout
				{
					rowSpacing: Theme.rowGroupSpacing                    
					GroupBox
					{
						RadioButton { value: "hyper-g";			text: qsTr("Hyper-g");			id: hyperg			}
						RadioButton { value: "hyper-g-laplace";	text: qsTr("Hyper-g-Laplace");	id: hyperglaplace	}
						RadioButton { value: "hyper-g-n";		text: qsTr("Hyper-g-n");		id: hypergn			}
					}
					IntegerField
					{
						name: "alpha";
						text: qsTr("alpha");
						enabled: hyperg.checked || hyperglaplace.checked || hypergn.checked
						defaultValue: 3
					}
					RadioButton { value: "JZS"; text: qsTr("JZS"); checked: true; id: jzs }
					DoubleField
					{
						name: "rScale"
						text: qsTr("r scale")
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
					name: "modelPrior"
					title: qsTr("Model prior")
					GridLayout
					{
						rowSpacing: Theme.rowGroupSpacing
						RadioButton { value: "beta.binomial"; text: qsTr("Beta binomial"); checked: true; id: betabinomial }
						RowLayout
						{
							enabled: betabinomial.checked
							DoubleField { name: "betaBinomialParamA"; text: qsTr("a");  defaultValue: 1 }
							DoubleField { name: "betaBinomialParamB"; text: qsTr("b");  defaultValue: 1 }
						}
						RadioButton { value: "Bernoulli"; text: qsTr("Bernouilli"); id: bernoulli }
						DoubleField { name: "bernoulliParam"; text: qsTr("p"); defaultValue: 0.5; enabled: bernoulli.checked; doubleValidator { top: 1; decimals: 2 } }
					}
					RadioButton {  value: "uniform"; text: qsTr("Uniform") }
				}
				
				RadioButtonGroup
				{
					name: "samplingMethod"
					title: qsTr("Sampling method")
					GridLayout
					{
						rowSpacing: Theme.rowGroupSpacing
						RadioButton { value: "BAS"; text: qsTr("BAS"); checked: true; id: bas }
						IntegerField { name: "numberOfModels"; text: qsTr("No. models"); defaultValue: 0; enabled: bas.checked; intValidator.top: 100000000 }
						RadioButton { value: "MCMC"; text: qsTr("MCMC"); id: mcmc }
						IntegerField { name: "iterationsMCMC"; text: qsTr("No. samples"); defaultValue: 0; enabled: mcmc.checked; intValidator.top: 100000000 }
					}
				}
				
				GroupBox
				{
					title: qsTr("Numerical accuracy")
					IntegerField { name: "nSimForCRI"; text: qsTr("No. samples for credible interval"); defaultValue: 1000; intValidator { bottom: 100; top: 1000000 } }
				}
			}
		}
	}
	
}
