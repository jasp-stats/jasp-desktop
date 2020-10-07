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
import JASP 1.0
Form
{
	property var listVisibility :
	{
		"input_t" :	{ values: ["cohensd"]					, id: input_t	, check2Sample: false },
		"input_SE": { values: ["cohensd", "general"]		, id: input_SE	, check2Sample: false },
		"input_CI":	{ values: ["cohensd", "general", "OR"]	, id: input_CI	, check2Sample: false },
		"input_N" :	{ values: ["cohensd", "correlation"]	, id: input_N	, check2Sample: false },
		"input_N1": { values: ["cohensd"]					, id: input_N1	, check2Sample: true  },
		"input_N2": { values: ["cohensd"]					, id: input_N2	, check2Sample: true  },
	}

	function checkListVisibility(name)
	{
		var check = (listVisibility[name]["check2Sample"] ? cohensd_twoSample.checked : true)

		return check && listVisibility[name]["values"].includes(measures.value);
	}

	RadioButtonGroup
	{
		id:						measures
		Layout.columnSpan:		2
		name:					"measures"
		radioButtonsOnSameRow:	true
		columns:				2

		onValueChanged:
		{
			if(measures_correlation.checked)
				advanced_mu_transform_cohens_d.click()
			else if (measures_OR.checked)
				advanced_mu_transform_log_OR.click()

			for (var inputName in listVisibility)
			{
				if (!checkListVisibility(inputName) && listVisibility[inputName]["id"].count > 0)
					listVisibility[inputName]["id"].itemDoubleClicked(0)
			}

		}

		RadioButton
		{
			label: qsTr("Cohen's d / t-statistics & N / SE")
			value: "cohensd"
			id: 	measures_cohensd
			checked:true
		}

		RadioButton
		{
			label: qsTr("Correlations & N")
			value: "correlation"
			id: 	measures_correlation
		}

		RadioButton
		{
			label: qsTr("Odds ratios & CI")
			value: "OR"
			id: 	measures_OR
		}

		RadioButton
		{
			label: qsTr("Effect sizes & SE")
			value: "general"
			id: 	measures_general
		}

		RadioButton
		{
			label: qsTr("Fitted model")
			value: "fitted"
			id: 	measures_fitted
		}
	}

	FileSelector
	{
		name:			"fitted_path"
		label:  		qsTr("Path to the fitted model")
		filter:			"*.RDS"
		save:			false
		visible:		measures_fitted.checked
	}

	VariablesForm
	{
		preferredHeight:	400 * preferencesModel.uiScale
		visible:			!measures_fitted.checked

		AvailableVariablesList {name: "variablesList"}

		AssignedVariablesList
		{
			id: 			input_ES
			name: 			"input_ES"
			enabled: 		input_t.count == 0
			title: 			if (measures_cohensd.checked){
				qsTr("Cohen's d")
			} else if (measures_correlation.checked) {
				qsTr("Correlation")
			} else if (measures_OR.checked) {
				qsTr("Odds Ratio")
			} else {
				qsTr("Effect Size")
			}
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name: 			"input_t"
			id: 			input_t
			enabled: 		input_ES.count == 0
			title: 			qsTr("t-statistic")
			singleVariable: true
			allowedColumns: ["scale"]
			visible:		checkListVisibility(name)
		}

		AssignedVariablesList
		{
			id: 			input_SE
			enabled: 		input_CI.count == 0 && input_N.count == 0 && input_N1.count == 0 && input_N2.count == 0 && input_t.count == 0
			name: 			"input_SE"
			title: 			qsTr("Effect Size Standard Error")
			singleVariable: true
			allowedColumns: ["scale"]
			visible:		checkListVisibility(name)
		}

		AssignedPairsVariablesList
		{
			id: 			input_CI
			enabled: 		input_SE.count == 0 && input_N.count == 0 && input_N1.count == 0 && input_N2.count == 0 && input_t.count == 0
			name: 			"input_CI"
			title: 			qsTr("95% CI Lower and Upper Bound")
			singleVariable: true
			allowedColumns: ["scale"]
			visible:		checkListVisibility(name)
		}

		AssignedVariablesList
		{
			id: 			input_N
			enabled: 		input_SE.count == 0 && input_CI.count == 0 && input_N1.count == 0 && input_N2.count == 0
			name: 			"input_N"
			title: 			qsTr("N")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		checkListVisibility(name)
		}

		AssignedVariablesList
		{
			id: 			input_N1
			enabled: 		input_SE.count == 0 && input_CI.count == 0 && input_N.count == 0
			name: 			"input_N1"
			title: 			qsTr("N (group 1)")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		 checkListVisibility(name)
		}

		AssignedVariablesList
		{
			id: 			input_N2
			enabled: 		input_SE.count == 0 && input_CI.count == 0 && input_N.count == 0
			name: 			"input_N2"
			title: 			qsTr("N (group 2)")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		 checkListVisibility(name)
		}

		AssignedVariablesList
		{
			name: 			"input_labels"
			title: 			qsTr("Study Labels")
			singleVariable:	true
			allowedColumns: ["nominal","nominalText"]
		}
	}

	RadioButtonGroup
	{
		name:					"cohensd_testType"
		visible:				measures_cohensd.checked
		radioButtonsOnSameRow:	true

		RadioButton
		{
			label:		qsTr("One-sample t-tests")
			value:		"one.sample"
			id:			cohensd_oneSample
		}
		RadioButton
		{
			label:		qsTr("Two-sample t-tests")
			value: 		"two.sample"
			id:			cohensd_twoSample
			checked:	true
		}
	}


	//// Priors ////
	Section
	{
		title: 			qsTr("Models")
		columns:		1

		RadioButtonGroup
		{
			name:		"effect_direction"
			title:		qsTr("Expected effect size direction")

			RadioButton
			{
				value:		"positive"
				label:		qsTr("Positive")
				checked: 	true
			}

			RadioButton
			{
				value:		"negative"
				label:		qsTr("Negative")
			}
		}

		CheckBox
		{
			name:		"priors_plot"
			label:		qsTr("Plot priors")
		}

		// mu prior
		ColumnLayout
		{
			spacing:				0
			Layout.preferredWidth:	parent.width
			visible:				!measures_fitted.checked

			Label { text: qsTr("Effect"); Layout.preferredHeight: 20 * preferencesModel.uiScale}


			RowLayout
			{
				Label { text: qsTr("Distribution");	Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
				Label { text: qsTr("Parameters");	Layout.preferredWidth: 155 * preferencesModel.uiScale }
				Label { text: qsTr("Truncation");	Layout.preferredWidth: 150 * preferencesModel.uiScale }
				Label { text: qsTr("Prior Odds") }
			}
			ComponentsList
			{
				name:					"priors_mu"
				optionKey:				"name"
				defaultValues: 			[{"type": "normal"}]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 140 * preferencesModel.uiScale
						DropDown
						{
							id: typeMuItem
							name: "type"
							useExternalBorder: true
							values:
							[
								{ label: qsTr("Normal(μ,σ)"),			value: "normal"},
								{ label: qsTr("Student-t(μ,σ,v)"),		value: "t"},
								{ label: qsTr("Cauchy(x₀,θ)"),			value: "cauchy"},
								{ label: qsTr("Gamma(α,β)"),			value: "gamma_ab"},
								{ label: qsTr("Gamma(k,θ)"),			value: "gamma_k0"},
								{ label: qsTr("Inverse-Gamma(α,β)"),	value: "invgamma"},
								{ label: qsTr("Spike(x₀)"),				value: "spike"},
								{ label: qsTr("Uniform(a,b)"),			value: "uniform"}
							]
						}
					}

					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 155 * preferencesModel.uiScale
						FormulaField
						{
							label:				"μ "
							name:				"parMean"
							visible:			typeMuItem.currentValue === "normal"		||
												typeMuItem.currentValue === "t"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"x₀"
							name:				"parLocation"
							visible:			typeMuItem.currentValue === "cauchy"	||
												typeMuItem.currentValue === "spike"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"σ"
							name:				"parScale"
							visible:			typeMuItem.currentValue === "normal"		||
												typeMuItem.currentValue === "t"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"k "
							name:				"parShape"
							visible:			typeMuItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
						}
						FormulaField
						{
							label:				"θ"
							name:				"parScale2"
							visible:			typeMuItem.currentValue === "cauchy"	||
												typeMuItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"ν"
							name:				"parDF"
							visible:			typeMuItem.currentValue === "t"
							value:				"2"
							min:				1
							inclusive:			JASP.MinOnly
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"α "
							name:				"parAlpha"
							visible:			typeMuItem.currentValue === "gamma_ab"	||
												typeMuItem.currentValue === "invgamma"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"β"
							name:				"parBeta"
							visible:			typeMuItem.currentValue === "gamma_ab"	||
												typeMuItem.currentValue === "invgamma"
							value:				"0.15"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"a "
							name:				"parA"
							id:					parA
							visible:			typeMuItem.currentValue === "uniform"
							value:				"0"
							max:				parB.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"b"
							name:				"parB"
							id:					parB
							visible:			typeMuItem.currentValue === "uniform"
							value:				"1"
							min:				parA.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}

					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 150 * preferencesModel.uiScale
						FormulaField
						{
							id:					truncationMuLower
							label: 				qsTr("lower")
							name: 				"truncationLower"
							visible:			typeMuItem.currentValue !== "spike" && typeMuItem.currentValue !== "uniform"
							value:				if(typeMuItem.currentValue === "gamma_k0" || typeMuItem.currentValue === "gamma_ab" || typeMuItem.currentValue === "invgamma"){ 0 } else "-Inf"
							min:				if(typeMuItem.currentValue === "gamma_k0" || typeMuItem.currentValue === "gamma_ab" || typeMuItem.currentValue === "invgamma"){ 0 } else "-Inf"
							max: 				truncationMuUpper.value
							inclusive: 			JASP.MinOnly
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							id:					truncationMuUpper
							label: 				qsTr("upper")
							name: 				"truncationUpper"
							visible:			typeMuItem.currentValue !== "spike" && typeMuItem.currentValue !== "uniform"
							value:				"Inf"
							min: 				truncationMuLower ? truncationMuLower.value : 0
							inclusive: 			JASP.MaxOnly
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
					FormulaField
					{
						label: 				qsTr("Odds")
						name: 				"priorOdds"
						value:				"1"
						min: 				0
						inclusive: 			JASP.None
						fieldWidth:			40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder:			true
					}
				}
			}
		}


		// tau prior
		ColumnLayout
		{
			spacing: 				0
			Layout.preferredWidth:	parent.width
			visible:				!measures_fitted.checked

			Label { text: qsTr("Heterogeneity"); Layout.preferredHeight: 20 * preferencesModel.uiScale}

			RowLayout
			{

				Label { text: qsTr("Distribution");	Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
				Label { text: qsTr("Parameters");	Layout.preferredWidth: 155 * preferencesModel.uiScale }
				Label { text: qsTr("Truncation");	Layout.preferredWidth: 150 * preferencesModel.uiScale }
				Label { text: qsTr("Prior Odds") }
			}
			ComponentsList
			{
				name:					"priors_tau"
				optionKey:				"name"
				defaultValues: 			[{"type": "invgamma"}]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 140 * preferencesModel.uiScale
						DropDown
						{
							id: typeTauItem
							name: "type"
							useExternalBorder: true
							values:
							[
								{ label: qsTr("Normal(μ,σ)"),			value: "normal"},
								{ label: qsTr("Student-t(μ,σ,v)"),		value: "t"},
								{ label: qsTr("Cauchy(x₀,θ)"),			value: "cauchy"},
								{ label: qsTr("Gamma(α,β)"),			value: "gamma_ab"},
								{ label: qsTr("Gamma(k,θ)"),			value: "gamma_k0"},
								{ label: qsTr("Inverse-Gamma(α,β)"),	value: "invgamma"},
								{ label: qsTr("Spike(x₀)"),				value: "spike"},
								{ label: qsTr("Uniform(a,b)"),			value: "uniform"}
							]
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 155 * preferencesModel.uiScale

						FormulaField
						{
							label:				"μ "
							name:				"parMean"
							visible:			typeTauItem.currentValue === "normal"	||
												typeTauItem.currentValue === "t"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"x₀"
							name:				"parLocation"
							visible:			typeTauItem.currentValue === "cauchy"	||
												typeTauItem.currentValue === "spike"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"σ"
							name:				"parScale"
							visible:			typeTauItem.currentValue === "normal"	||
												typeTauItem.currentValue === "t"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"k "
							name:				"parShape"
							visible:			typeTauItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"θ"
							name:				"parScale2"
							visible:			typeTauItem.currentValue === "cauchy"	||
												typeTauItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"ν"
							name:				"parDF"
							visible:			typeTauItem.currentValue === "t"
							value:				"2"
							min:				1
							inclusive:			JASP.MinOnly
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"α "
							name:				"parAlpha"
							visible:			typeTauItem.currentValue === "gamma_ab"		||
												typeTauItem.currentValue === "invgamma"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"β"
							name:				"parBeta"
							visible:			typeTauItem.currentValue === "gamma_ab"	||
												typeTauItem.currentValue === "invgamma"
							value:				"0.15"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"a "
							name:				"parA"
							id:					parATau
							visible:			typeTauItem.currentValue === "uniform"
							value:				"0"
							min:				0
							max:				parBTau.value
							inclusive:			JASP.MinOnly
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"b"
							name:				"parB"
							id:					parBTau
							visible:			typeTauItem.currentValue === "uniform"
							value:				"1"
							min:				parATau.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 150 * preferencesModel.uiScale

						FormulaField
						{
							id:					truncationTauLower
							label: 				qsTr("lower")
							name: 				"truncationLower"
							value:				"0"
							min:				0
							max: 				truncationTauUpper.value
							inclusive: 			JASP.MinOnly
							visible:			typeTauItem.currentValue !== "spike" && typeTauItem.currentValue !== "uniform"
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							id:					truncationTauUpper
							label: 				qsTr("upper")
							name: 				"truncationUpper"
							value:				"Inf"
							min: 				truncationTauLower ? truncationTauLower.value : 0
							inclusive: 			JASP.MaxOnly
							visible:			typeTauItem.currentValue !== "spike" && typeTauItem.currentValue !== "uniform"
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
							Layout.rightMargin: 15
						}
					}
					FormulaField
					{
						label: 				qsTr("Odds")
						name: 				"priorOdds"
						value:				"1"
						min: 				0
						inclusive: 			JASP.None
						fieldWidth:			40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder:			true
					}
				}
			}
		}


		// omega prior
		ColumnLayout
		{
			spacing: 0
			Layout.preferredWidth:	parent.width
			visible:				!measures_fitted.checked

			Label { text: qsTr("Publication bias"); Layout.preferredHeight: 20 * preferencesModel.uiScale}

			RowLayout
			{

				Label { text: qsTr("Weight function");	Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
				Label { text: qsTr("Cut-points");		Layout.preferredWidth: 155 * preferencesModel.uiScale }
				Label { text: qsTr("Parameters");		Layout.preferredWidth: 150 * preferencesModel.uiScale }
				Label { text: qsTr("Prior Odds")}
			}
			ComponentsList
			{
				name:					"priors_omega"
				optionKey:				"name"
				defaultValues:
				[
					{"type": "Two-sided", "parCuts": "(.05)", "parAlpha": "(1,1)", "priorOdds": "1/2"},
					{"type": "Two-sided", "parCuts": "(.05, .10)", "parAlpha": "(1,1,1)", "priorOdds": "1/2"}
				]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 140 * preferencesModel.uiScale
						DropDown
						{
							id: typeOmegatem
							name: "type"
							useExternalBorder: true
							values: [
								{ label: qsTr("Two-sided"),			value: "Two-sided"},
								{ label: qsTr("One-sided (mon.)"),	value: "One-sided (mon.)"},
								{ label: qsTr("One-sided"),			value: "One-sided"},
								{ label: qsTr("None"),				value: "spike"}
							]

								onCurrentValueChanged: {
								if (currentValue !== "Two-sided")
								{
									parCuts.value = "(.05, .95)";
									parAlpha.value = "(1,1,1)";
									parCuts.editingFinished();
									parAlpha.editingFinished();
								}
								else if (parCuts.value !== '(.05)' && parCuts !== "(.05, .10)")
								{
									parCuts.value = "(.05, .10)";
									parCuts.editingFinished();
								}
							}
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 155 * preferencesModel.uiScale
						TextField
						{
							label:				qsTr("p-values")
							id:					parCuts
							name:				"parCuts"
							visible:			typeOmegatem.currentValue === "Two-sided"		||
												typeOmegatem.currentValue === "One-sided"		||
												typeOmegatem.currentValue === "One-sided (mon.)"
							value:				"(.05, .10)"
							fieldWidth: 		100 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 150 * preferencesModel.uiScale

						TextField
						{
							label:				"α "
							id:					parAlpha
							name:				"parAlpha"
							visible:			typeOmegatem.currentValue === "Two-sided"		||
												typeOmegatem.currentValue === "One-sided (mon.)"
							value:				"(1,1,1)"
							fieldWidth: 		70 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						TextField
						{
							label:				"α₁"
							name:				"parAlpha1"
							visible:			typeOmegatem.currentValue === "One-sided"
							value:				"(1,1)"
							fieldWidth: 		70 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						TextField
						{
							label:				"α₂"
							name:				"parAlpha2"
							visible:			typeOmegatem.currentValue === "One-sided"
							value:				"(1,1)"
							fieldWidth: 		50 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}
					FormulaField
					{
						label: 				qsTr("Odds")
						name: 				"priorOdds"
						value:				"1"
						min: 				0
						inclusive: 			JASP.None
						fieldWidth:			40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder:			true
					}
				}
			}
		}


		CheckBox
		{
			id:						priors_null
			name:					"priors_null"
			label:					qsTr("Set null priors")
			visible:				!measures_fitted.checked
		}


		// mu null prior
		ColumnLayout
		{
			spacing:				0
			Layout.preferredWidth:	parent.width
			visible:				priors_null.checked && !measures_fitted.checked

			Label { text: qsTr("Effect (null)"); Layout.preferredHeight: 20 * preferencesModel.uiScale}


			RowLayout
			{
				Label { text: qsTr("Distribution"); Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
				Label { text: qsTr("Parameters");	Layout.preferredWidth: 155 * preferencesModel.uiScale }
				Label { text: qsTr("Truncation");	Layout.preferredWidth: 150 * preferencesModel.uiScale }
				Label { text: qsTr("Prior Odds") }
			}
			ComponentsList
			{
				name:					"priors_mu_null"
				optionKey:				"name"
				defaultValues: 			[{"type": "spike"}]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 140 * preferencesModel.uiScale
						DropDown
						{
							id: typeMuNullItem
							name: "type"
							useExternalBorder: true
							values:
							[
								{ label: qsTr("Normal(μ,σ)"),			value: "normal"},
								{ label: qsTr("Student-t(μ,σ,v)"),		value: "t"},
								{ label: qsTr("Cauchy(x₀,θ)"),			value: "cauchy"},
								{ label: qsTr("Gamma(α,β)"),			value: "gamma_ab"},
								{ label: qsTr("Gamma(k,θ)"),			value: "gamma_k0"},
								{ label: qsTr("Inverse-Gamma(α,β)"),	value: "invgamma"},
								{ label: qsTr("Spike(x₀)"),				value: "spike"},
								{ label: qsTr("Uniform(a,b)"),			value: "uniform"}
							]
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 155 * preferencesModel.uiScale
						FormulaField
						{
							label:				"μ "
							name:				"parMean"
							visible:			typeMuNullItem.currentValue === "normal"		||
												typeMuNullItem.currentValue === "t"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"x₀"
							name:				"parLocation"
							visible:			typeMuNullItem.currentValue === "cauchy"	||
												typeMuNullItem.currentValue === "spike"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"σ"
							name:				"parScale"
							visible:			typeMuNullItem.currentValue === "normal"		||
												typeMuNullItem.currentValue === "t"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"k "
							name:				"parShape"
							visible:			typeMuNullItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
						}
						FormulaField
						{
							label:				"θ"
							name:				"parScale2"
							visible:			typeMuNullItem.currentValue === "cauchy"	||
												typeMuNullItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"ν"
							name:				"parDF"
							visible:			typeMuNullItem.currentValue === "t"
							value:				"2"
							min:				1
							inclusive:			JASP.MinOnly
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"α "
							name:				"parAlpha"
							visible:			typeMuNullItem.currentValue === "gamma_ab"	||
												typeMuNullItem.currentValue === "invgamma"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"β"
							name:				"parBeta"
							visible:			typeMuNullItem.currentValue === "gamma_ab"	||
												typeMuNullItem.currentValue === "invgamma"
							value:				"0.15"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"a "
							name:				"parA"
							id:					parAMuNull
							visible:			typeMuNullItem.currentValue === "uniform"
							value:				"0"
							max:				parBMuNull.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"b"
							name:				"parB"
							id:					parBMuNull
							visible:			typeMuNullItem.currentValue === "uniform"
							value:				"1"
							min:				parAMuNull.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 150 * preferencesModel.uiScale
						FormulaField
						{
							id:					truncationMuNullLower
							label: 				qsTr("lower")
							name: 				"truncationLower"
							visible:			typeMuNullItem.currentValue !== "spike" && typeMuNullItem.currentValue !== "uniform"
							value:				if(typeMuNullItem.currentValue === "gamma_k0" || typeMuNullItem.currentValue === "gamma_ab" || typeMuNullItem.currentValue === "invgamma"){ 0 } else "-Inf"
							min:				if(typeMuNullItem.currentValue === "gamma_k0" || typeMuNullItem.currentValue === "gamma_ab" || typeMuNullItem.currentValue === "invgamma"){ 0 } else "-Inf"
							max: 				truncationMuNullUpper.value
							inclusive: 			JASP.MinOnly
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							id:					truncationMuNullUpper
							label: 				qsTr("upper")
							name: 				"truncationUpper"
							visible:			typeMuNullItem.currentValue !== "spike" && typeMuNullItem.currentValue !== "uniform"
							value:				"Inf"
							min: 				truncationMuNullLower ? truncationMuNullLower.value : 0
							inclusive: 			JASP.MaxOnly
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
					FormulaField
					{
						label: 				qsTr("Odds")
						name: 				"priorOdds"
						value:				"1"
						min: 				0
						inclusive: 			JASP.None
						fieldWidth:			40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder:			true
					}
				}
			}
		}


		// tau null prior
		ColumnLayout
		{
			spacing: 				0
			Layout.preferredWidth:	parent.width
			visible:				priors_null.checked && !measures_fitted.checked

			Label { text: qsTr("Heterogeneity (null)"); Layout.preferredHeight: 20 * preferencesModel.uiScale}

			RowLayout
			{

				Label { text: qsTr("Distribution"); Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
				Label { text: qsTr("Parameters");	Layout.preferredWidth: 155 * preferencesModel.uiScale }
				Label { text: qsTr("Truncation");	Layout.preferredWidth: 150 * preferencesModel.uiScale }
				Label { text: qsTr("Prior Odds") }
			}
			ComponentsList
			{
				name:					"priors_tau_null"
				optionKey:				"name"
				defaultValues: 			[{"type": "spike"}]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 140 * preferencesModel.uiScale
						DropDown
						{
							id: typeTauNullItem
							name: "type"
							useExternalBorder: true
							values:
							[
								{ label: qsTr("Normal(μ,σ)"),			value: "normal"},
								{ label: qsTr("Student-t(μ,σ,v)"),		value: "t"},
								{ label: qsTr("Cauchy(x₀,θ)"),			value: "cauchy"},
								{ label: qsTr("Gamma(α,β)"),			value: "gamma_ab"},
								{ label: qsTr("Gamma(k,θ)"),			value: "gamma_k0"},
								{ label: qsTr("Inverse-Gamma(α,β)"),	value: "invgamma"},
								{ label: qsTr("Spike(x₀)"),				value: "spike"},
								{ label: qsTr("Uniform(a,b)"),			value: "uniform"}
							]
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 155 * preferencesModel.uiScale

						FormulaField
						{
							label:				"μ"
							name:				"parMean"
							visible:			typeTauNullItem.currentValue === "normal"	||
												typeTauNullItem.currentValue === "t"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"x₀"
							name:				"parLocation"
							visible:			typeTauNullItem.currentValue === "cauchy"	||
												typeTauNullItem.currentValue === "spike"
							value:				"0"
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"σ"
							name:				"parScale"
							visible:			typeTauNullItem.currentValue === "normal"	||
												typeTauNullItem.currentValue === "t"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"k"
							name:				"parShape"
							visible:			typeTauNullItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"θ"
							name:				"parScale2"
							visible:			typeTauNullItem.currentValue === "cauchy"	||
												typeTauNullItem.currentValue === "gamma_k0"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"ν"
							name:				"parDF"
							visible:			typeTauNullItem.currentValue === "t"
							value:				"2"
							min:				1
							inclusive:			JASP.MinOnly
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"α"
							name:				"parAlpha"
							visible:			typeTauNullItem.currentValue === "gamma_ab"		||
												typeTauNullItem.currentValue === "invgamma"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"β"
							name:				"parBeta"
							visible:			typeTauNullItem.currentValue === "gamma_ab"	||
												typeTauNullItem.currentValue === "invgamma"
							value:				"0.15"
							min:				0
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"a "
							name:				"parA"
							id:					parATauNull
							visible:			typeTauNullItem.currentValue === "uniform"
							value:				"0"
							min:				0
							max:				parBTauNull.value
							inclusive:			JASP.MinOnly
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						FormulaField
						{
							label:				"b"
							name:				"parB"
							id:					parBTauNull
							visible:			typeTauNullItem.currentValue === "uniform"
							value:				"1"
							min:				parATauNull.value
							inclusive:			JASP.None
							fieldWidth: 		40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 150 * preferencesModel.uiScale

						FormulaField
						{
							id:					truncationTauNullLower
							label: 				qsTr("lower")
							name: 				"truncationLower"
							value:				"0"
							min:				0
							max: 				truncationTauNullUpper.value
							inclusive: 			JASP.MinOnly
							visible:			typeTauNullItem.currentValue !== "spike" && typeTauNullItem.currentValue !== "uniform"
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							id:					truncationTauNullUpper
							label: 				qsTr("upper")
							name: 				"truncationUpper"
							value:				"Inf"
							min: 				truncationTauNullLower ? truncationTauNullLower.value : 0
							inclusive: 			JASP.MaxOnly
							visible:			typeTauNullItem.currentValue !== "spike" && typeTauNullItem.currentValue !== "uniform"
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
							Layout.rightMargin: 15
						}
					}
					FormulaField
					{
						label: 				qsTr("Odds")
						name: 				"priorOdds"
						value:				"1"
						min: 				0
						inclusive: 			JASP.None
						fieldWidth:			40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder:			true
					}
				}
			}
		}


		// omega null prior
		ColumnLayout
		{
			spacing: 0
			Layout.preferredWidth:	parent.width
			visible:				priors_null.checked && !measures_fitted.checked

			Label { text: qsTr("Publication bias (null)"); Layout.preferredHeight: 20 * preferencesModel.uiScale}

			RowLayout
			{

				Label { text: qsTr("Weight function");	Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
				Label { text: qsTr("Cut-points"); 		Layout.preferredWidth: 155 * preferencesModel.uiScale }
				Label { text: qsTr("Parameters");		Layout.preferredWidth: 150 * preferencesModel.uiScale }
				Label { text: qsTr("Prior Odds") }
			}
			ComponentsList
			{
				name:					"priors_omega_null"
				optionKey:				"name"
				defaultValues:			[{"type": "spike"}]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 140 * preferencesModel.uiScale
						DropDown
						{
							id: typeOmegaNullItem
							name: "type"
							useExternalBorder: true
							values: [
								{ label: qsTr("Two-sided"),			value: "Two-sided"},
								{ label: qsTr("One-sided (mon.)"),	value: "One-sided (mon.)"},
								{ label: qsTr("One-sided"),			value: "One-sided"},
								{ label: qsTr("None"),				value: "spike"}
							]
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 155 * preferencesModel.uiScale
						TextField
						{
							label:				qsTr("p-values")
							name:				"parCuts"
							visible:			typeOmegaNullItem.currentValue === "Two-sided"		||
												typeOmegaNullItem.currentValue === "One-sided"		||
												typeOmegaNullItem.currentValue === "One-sided (mon.)"
							value:				if(typeOmegaNullItem.currentValue === "Two-sided") {"(.05, .10)"} else {"(.05, .95)"}
							fieldWidth: 		100 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 150 * preferencesModel.uiScale

						TextField
						{
							label:				"α "
							name:				"parAlpha"
							visible:			typeOmegaNullItem.currentValue === "Two-sided"		||
												typeOmegaNullItem.currentValue === "One-sided (mon.)"
							value:				"(1,1,1)"
							fieldWidth: 		70 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						TextField
						{
							label:				"α₁"
							name:				"parAlpha1"
							visible:			typeOmegaNullItem.currentValue === "One-sided"
							value:				"(1,1)"
							fieldWidth: 		70 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
						TextField
						{
							label:				"α₂"
							name:				"parAlpha2"
							visible:			typeOmegaNullItem.currentValue === "One-sided"
							value:				"(1,1)"
							fieldWidth: 		50 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder: 		true
						}
					}
					FormulaField
					{
						label: 				qsTr("Odds")
						name: 				"priorOdds"
						value:				"1"
						min: 				0
						inclusive: 			JASP.None
						fieldWidth:			40 * preferencesModel.uiScale
						useExternalBorder:	false
						showBorder:			true
					}
				}
			}
		}

	}


	//// Options section ////
	Section
	{
		title: qsTr("Inference")

		Group
		{

			CheckBox
			{
				label:		qsTr("Conditional estimates")
				name:		"results_conditional"
			}

			CheckBox
			{
				columns:	2
				label:		qsTr("Models overview")
				name:		"results_models"

				RadioButtonGroup
				{
					name: "results_models_BF"
					title: qsTr("BF")

					RadioButton
					{
						name: 		"inclusion"
						label: 		qsTr("Inclusion")
						checked: 	true
					}

					RadioButton
					{
						name: 		"best"
						label: 		qsTr("vs Best")
					}

					RadioButton
					{
						name: 		"previous"
						label: 		qsTr("vs Previous")
						enabled:	results_models_order_marglik.checked
					}
				}

				RadioButtonGroup
				{
					name: 		"results_models_order"
					title:		qsTr("Order")

					RadioButton
					{
						name: 		"default"
						label: 		qsTr("Priors")
						checked:	true
					}

					RadioButton
					{
						name: 		"marglik"
						label: 		qsTr("Marginal likelihood")
						id:			results_models_order_marglik
					}

					RadioButton
					{
						name: 		"posterior"
						label: 		qsTr("Posterior prob.")

					}
				}
			}

			CheckBox
			{
				label:		qsTr("Individual models")
				name:		"results_individual"

				CheckBox
				{
					label:		qsTr("Single model")
					name:		"results_individual_single"
					childrenOnSameRow: true
					IntegerField
					{
						name:	"results_individual_single_number"
						defaultValue:	1
					}
				}
			}

		}

		Group
		{

			BayesFactorType{}

			CIField
			{
				name: "results_CI"
				label: qsTr("CI width")
			}

			CheckBox
			{
				label:		qsTr("Estimated studies' effects")
				name:		"results_theta"
			}


		}



	}

	//// Plots section ////
	Section
	{
		title: 		qsTr("Plots")

		Group
		{
			title:	qsTr("Pooled estimates")
			columns: 1

			CheckBox
			{
				columns:	2
				label:		qsTr("Forest plot")
				name:		"plots_theta"

				RadioButtonGroup
				{
					name: "plots_theta_show"
					title: qsTr("Show")

					RadioButton
					{
						name: 		"observed"
						label: 		qsTr("Observed")
						checked: 	true
					}

					RadioButton
					{
						name: 		"estimated"
						label: 		qsTr("Estimated")
					}

					RadioButton
					{
						name: 		"both"
						label: 		qsTr("Both")
					}
				}

				RadioButtonGroup
				{
					name: 		"plots_theta_order"
					title:		qsTr("Order")

					RadioButton
					{
						name: 	"ascending"
						label: 	qsTr("Ascending")
					}

					RadioButton
					{
						name: 	"descending"
						label: 	qsTr("Descending")
					}

					RadioButton
					{
						name: 	"labels"
						label: 	qsTr("Row order")
						checked:true
					}
				}
			}

			CheckBox
			{
				label:	qsTr("Effect")
				name:	"plots_mu"
			}

			CheckBox
			{
				label:	qsTr("Heterogeneity")
				name:	"plots_tau"
			}

			CheckBox
			{
				label:	qsTr("Weights")
				name:	"plots_omega"

				CheckBox
				{
					label:	qsTr("Weight function")
					name:	"plots_omega_function"
					checked:true

					CheckBox
					{
						name:	"rescale_weightfunction"
						text:	qsTr("Rescale x-axis")
					}
				}
			}
		}

		Group
		{
			title: " " // Add a line to align with the first column
			columns: 1

			RadioButtonGroup
			{
				name:	"plots_type"
				title:	qsTr("Type")
				RadioButton
				{
					value:	"averaged"
					label:	qsTr("Model averaged")
					checked:true
				}
				RadioButton
				{
					value:	"conditional"
					label:	qsTr("Conditional")
				}

			}

			CheckBox
			{
				label:	qsTr("Show priors")
				name:	"plots_priors"
				checked:true
			}
/*************************************************** TODO: enable once/if CI are made available
			RadioButtonGroup
			{
				name:	"plots_estimate"
				title:	qsTr("Point estimate")
				RadioButton
				{
					value:	"mean"
					label:	qsTr("Mean")
					checked:true
				}
				RadioButton
				{
					value:	"median"
					label:	qsTr("Median")
				}
			}

			CIField
			{
				name: "plots_CI"
				label: qsTr("CI width")
			}
****************************************************/
		}

		Divider { }

		Group
		{
			title:	qsTr("Individual models")
			columns: 1

			CheckBox
			{
				label:	qsTr("Effect")
				name:	"plots_individual_mu"
			}

			CheckBox
			{
				label:	qsTr("Heterogeneity")
				name:	"plots_individual_tau"
			}

			CheckBox
			{
				label:	qsTr("Weights")
				name:	"plots_individual_omega"
			}
		}

		Group
		{
			title: " "
			columns: 1

			CheckBox
			{
				name:	"plots_type_individual_conditional"
				label:	qsTr("Conditional models only")
				checked:true
			}

			RadioButtonGroup
			{
				name: 		"plots_type_individual_by"
				title:		qsTr("Order")
				RadioButton
				{
					name: 	"model"
					label: 	qsTr("Model number")
					checked:true
				}

				RadioButton
				{
					name: 	"prob"
					label: 	qsTr("Posterior prob.")
				}

				RadioButton
				{
					name: 	"marglik"
					label: 	qsTr("Marginal likelihood")
				}
			}

			RadioButtonGroup
			{
				name: 		"plots_type_individual_order"
				columns: 	2
				RadioButton
				{
					name: 	"ascending"
					label: 	qsTr("Ascending")
				}

				RadioButton
				{
					name: 	"descending"
					label: 	qsTr("Descending")
				}
			}
		}
	}

	//// Diagnostics section ////
	Section
	{
		title: qsTr("MCMC Diagnostics")

		CheckBox
		{
			Layout.columnSpan: 2
			label:		qsTr("Overview")
			name:		"diagnostics_overview"

				CheckBox
				{
					label:		qsTr("Include theta")
					name:		"diagnostics_overview_theta"
				}
		}

		Group
		{
			title:			qsTr("Plot")
			CheckBox
			{
				label:		qsTr("Effect")
				name:		"diagnostics_mu"
			}

			CheckBox
			{
				label:		qsTr("Heterogeneity")
				name:		"diagnostics_tau"
			}

			CheckBox
			{
				label:		qsTr("Weights")
				name:		"diagnostics_omega"
			}

			CheckBox
			{
				label:		qsTr("Estimated studies' effects")
				name:		"diagnostics_theta"
			}
		}

		Group
		{
			title:			qsTr("Type")
			CheckBox
			{
				label:		qsTr("Trace")
				name:		"diagnostics_trace"
			}

			CheckBox
			{
				label:		qsTr("Autocorrelation")
				name:		"diagnostics_autocorrelation"
			}

			CheckBox
			{
				label:		qsTr("Posterior samples densities")
				name:		"diagnostics_samples"
			}

		}

		CheckBox
		{
			label:		qsTr("Single model")
			name:		"diagnostics_single"
			childrenOnSameRow: true
			IntegerField
			{
				name:	"diagnostics_single_model"
				defaultValue:	1
			}
		}

		CheckBox
		{
			label:		qsTr("Transform estimates")
			name:		"diagnostics_transformed"
			checked:	true
			visible:	measures_correlation.checked | measures_fitted.checked
		}

	}

	//// Advanced section for prior model probabilities sampling settings ////
	Section
	{
		columns: 		2
		title: 			qsTr("Advanced")

		/* This will work with JASP 0.15 (+ delete line 31-35)
		DropDown
		{
			Layout.columnSpan: 2
			enabled:	measures_correlation.checked || measures_OR.checked
			label:		qsTr("Transform correlations")
			name:		"advanced_mu_transform"
			id:			advanced_mu_transform
			values:		measures_correlation.checked ?
			[
				{ label: qsTr("Cohen's d"),		value: "cohens_d"},
				{ label: qsTr("Fisher's z"),	value: "fishers_z"}
			]:	[
				{ label: qsTr("log(OR)"),		value: "log_OR"},
				{ label: qsTr("Cohen's d"),		value: "cohens_d"}
			]		
		}
		*/

		RadioButtonGroup
		{
			Layout.columnSpan:		2
			name:					"advanced_mu_transform"
			title:					qsTr("Transform effect sizes")
			id:						advanced_mu_transform
			enabled:				measures_correlation.checked || measures_OR.checked

			RadioButton
			{
				label:		qsTr("log(OR)")
				value:		"log_OR"
				visible:	measures_OR.checked
				id:			advanced_mu_transform_log_OR
			}

			RadioButton
			{
				label:		qsTr("Cohen's d")
				value:		"cohens_d"
				visible:	measures_correlation.checked || measures_OR.checked
				id:			advanced_mu_transform_cohens_d
			}

			RadioButton
			{
				label:		qsTr("Fisher's z")
				value:		"fishers_z"
				visible:	measures_correlation.checked
				id:			advanced_mu_transform_fishers_z
			}
		}

		Group
		{
			rowSpacing: 10 * preferencesModel.uiScale

			Group
			{
				title: 		qsTr("Estimation settings (MCMC)")

				IntegerField
				{
					name:			"advanced_adapt"
					label:			qsTr("Adaptation")
					defaultValue:	1000
					min:			100
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advanced_burnin"
					label:			qsTr("Burnin")
					defaultValue:	5000
					min:			1000
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advanced_iteration"
					label:			qsTr("Iterations")
					defaultValue:	10000
					min:			4000
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advanced_chains"
					label:			qsTr("Chains")
					defaultValue:	3
					min:			2
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advanced_thin"
					label:			qsTr("Thin")
					defaultValue:	1
					min:			1
					fieldWidth:		55 * preferencesModel.uiScale
				}

			}

			IntegerField
			{
				label:  		qsTr("Bridge sampling iterations:")
				name:     		"advanced_bridge_iter"
				defaultValue: 	10000
				max:			1000000
				fieldWidth: 	60
			}

			SetSeed{}
		}

		Group
		{
			rowSpacing: 10 * preferencesModel.uiScale

			CheckBox
			{
				label: 		qsTr("Autofit")
				name:		"advanced_autofit"
				checked:	false

				Row
				{
					IntegerField
					{
						name:			"advanced_autofit_time"
						label:			qsTr("Maximum fitting time")
						defaultValue:	1
						min:			0
					}

					DropDown
					{
						name:	"advanced_autofit_time_unit"
						values:
						[
							{ label: qsTr("hours"),				value: "hours"},
							{ label: qsTr("minutes"),			value: "minutes"},
							{ label: qsTr("seconds"),			value: "seconds"}

						]
					}
				}

				PercentField
				{
					id:				advanced_autofit_error
					name:			"advanced_autofit_error"
					label:			qsTr("Target margin of error")
					defaultValue:	1
					decimals:		1
				}


			}

			CheckBox
			{
				label:		qsTr("Exclude models")
				name:		"advanced_omit"
				checked:	false

				Group
				{
					columns: 2

					CheckBox
					{
						id:					advanced_omit_error
						name:				"advanced_omit_error"
						label:				qsTr("error % >")
						checked:			false
					}

					PercentField
					{
						enabled:		advanced_omit_error.checked
						name: 			"advanced_omit_error_value"
						defaultValue: 	1
						decimals:		1
						fieldWidth:		jaspTheme.numericFieldWidth
					}

					CheckBox
					{
						id:					advanced_omit_rhat
						name:				"advanced_omit_rhat"
						label:				qsTr("R-hat >")
						checked:			false
					}

					DoubleField
					{
						enabled:		advanced_omit_rhat.checked
						name: 			"advanced_omit_rhat_value"
						defaultValue: 	1.05
						min:			1
					}

					CheckBox
					{
						id:				advanced_omit_ESS
						name:			"advanced_omit_ESS"
						label:			qsTr("Estimated sample size <")
					}

					DoubleField
					{
						enabled:		advanced_omit_ESS.checked
						name: 			"advanced_omit_ESS_value"
						defaultValue:	500
						min: 			1
					}

					CheckBox
					{
						Layout.columnSpan: 2
						label:			qsTr("Include theta")
						name:			"advanced_omit_theta"
					}
				}

				DropDown
				{
					label:		qsTr("Redistribute prior probability")
					name:		"advanced_omit_prior"
					values:
					[
						{ label: qsTr("Conditional models"),	value: "conditional"},
						{ label: qsTr("Model space"),			value: "overal"}
					]
				}
			}

			DropDown
			{
				label: qsTr("Control")
				name: "advanced_control"
				values:
				[
					{ label: qsTr("Clever refitting"),	value: "clever",	default: true},
					{ label: qsTr("Do not refit"),		value: "no_refit"},
					{ label: qsTr("Always refit"),		value: "refit"}
				]
			}
		}

		FileSelector
		{
			Layout.columnSpan:	2
			label: 				qsTr("Save the fitted model")
			name:				"save_path"
			filter:				"*.RDS"
			save:				true
		}


	}

}
