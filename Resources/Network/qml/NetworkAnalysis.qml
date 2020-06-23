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
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables";			title: qsTr("Dependent Variables"); suggestedColumns: ["ordinal", "scale"]; id: networkVariables}
		AssignedVariablesList { name: "groupingVariable";	title: qsTr("Split"); singleVariable: true; suggestedColumns: ["ordinal", "nominal"] }
	}

	DropDown
	{
		id: estimator
		name: "estimator"
		label: qsTr("Estimator")
		Layout.columnSpan: 2
		values: [
			{ value: "EBICglasso",		label: "EBICglasso"			},
			{ value: "cor",				label: qsTr("Correlation")		},
			{ value: "pcor",			label: qsTr("Partial Correlation")},
			{ value: "IsingFit",		label: "IsingFit"			},
			{ value: "IsingSampler",	label: "IsingSampler"		},
			{ value: "huge",			label: qsTr("huge")			},
			{ value: "adalasso",		label: "adalasso"			},
			{ value: "mgm",				label: "mgm"				}
		]
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox { name: "plotNetwork";		label: qsTr("Network plot")								}
		CheckBox { name: "plotCentrality";	label: qsTr("Centrality plot");		id: plotCentrality	}
		CheckBox { name: "plotClustering";	label: qsTr("Clustering plot")							}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { name: "tableCentrality";		label: qsTr("Centrality table")	}
		CheckBox { name: "tableClustering";		label: qsTr("Clustering table")	}
		CheckBox { name: "tableWeightsMatrix";	label: qsTr("Weights matrix")	}
	}

	Section
	{
		title: qsTr("Analysis Options - ") + estimator.currentText

		RadioButtonGroup
		{
			name: "correlationMethod"
			title: qsTr("Correlation Method")
			visible: [0, 1, 2].includes(estimator.currentIndex)
			RadioButton { value: "auto";	label: qsTr("Auto"); checked: true	}
			RadioButton { value: "cor";		label: qsTr("Cor")					}
			RadioButton { value: "cov";		label: qsTr("Cov")					}
			RadioButton { value: "npn";		label: qsTr("Npn")					}
		}

		RadioButtonGroup
		{
			name: "normalizeCentrality"
			title: qsTr("Centrality Measures")
			visible: estimator.currentIndex === 0
			RadioButton { value: "normalized";	label: qsTr("Normalized"); checked: true }
			RadioButton { value: "relative" ;	label: qsTr("Relative")					}
			RadioButton { value: "raw";			label: qsTr("Raw")						}
		}

		Group
		{
			title: qsTr("Network")
			visible: estimator.currentIndex === 0
			CheckBox { name: "weightedNetwork"; label: qsTr("Weighted"); checked: true	}
			CheckBox { name: "signedNetwork";	label: qsTr("Signed");	checked: true	}
		}

		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			visible: [0, 1, 2].includes(estimator.currentIndex)
			RadioButton { value: "pairwise";	label: qsTr("Exclude pairwise"); checked: true	}
			RadioButton { value: "listwise";	label: qsTr("Exclude listwise")					}
		}

		RadioButtonGroup
		{
			name: "sampleSize"
			title: qsTr("Sample Size")
			visible: estimator.currentIndex === 0
			RadioButton { value: "maximum";	label: qsTr("Maximum"); checked: true	}
			RadioButton { value: "minimim";	label: qsTr("Minimum")					}
		}

		RadioButtonGroup
		{
			name: "isingEstimator"
			title: qsTr("Ising Estimator")
			visible: estimator.currentIndex === 4
			RadioButton { value: "pseudoLikelihood";		label: qsTr("Pseudo-likelihood"); checked: true	}
			RadioButton { value: "univariateRegressions";	label: qsTr("Univariate regressions")			}
			RadioButton { value: "bivariateRegressions";	label: qsTr("Bivariate regressions")			}
			RadioButton { value: "logLinear";				label: qsTr("Loglinear")						}
		}

		RadioButtonGroup
		{
			name: "criterion"
			title: qsTr("Criterion")
			visible: [5, 7].includes(estimator.currentIndex)
			RadioButton { value: "ebic";	label: qsTr("EBIC"); checked: true	}
			RadioButton { value: "ric";		label: qsTr("RIC")					}
			RadioButton { value: "stars";	label: qsTr("STARS")				}
			RadioButton { value: "cv";		label: qsTr("CV")					}
		}

		RadioButtonGroup
		{
			name: "rule"
			title: qsTr("Rule")
			visible: [3, 7].includes(estimator.currentIndex)
			RadioButton { value: "and";	label: qsTr("AND"); checked: true	}
			RadioButton { value: "or";	label: qsTr("OR")					}
		}

		RadioButtonGroup
		{
			name: "split"
			title: qsTr("Split")
			visible: [3, 4].includes(estimator.currentIndex)
			RadioButton { value: "median";	label: qsTr("Median"); checked: true	}
			RadioButton { value: "mean";	label: qsTr("Mean")						}
		}

		Group
		{
			title: qsTr("Tuning Parameter")
			visible: [0, 3, 5, 7].includes(estimator.currentIndex)
			DoubleField { name: "tuningParameter"; label: qsTr("Value"); defaultValue: 0.5; max: 1 }
		}

		RadioButtonGroup
		{
			name: "thresholdBox"
			title: qsTr("Threshold")
			visible: [1, 2].includes(estimator.currentIndex)
			RadioButton
			{
				value: "value";	label: qsTr("Value"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "thresholdValue"; defaultValue: 0; max: 1000000000 }
			}
			RadioButton
			{
				value: "method"; label: qsTr("Method")
				childrenOnSameRow: true
				DropDown
				{
					name: "thresholdMethod"
					values: [
						{ label: qsTr("Significant"),	value: "sig"		},
						{ label: "Bonferroni",	value: "bonferroni"	},
						{ label: "Locfdr",		value: "locfdr"		},
						{ label: "Holm",		value: "holm"		},
						{ label: "Hochberg",	value: "hochberg"	},
						{ label: "Hommel",		value: "hommel"		},
						{ label: "BH",			value: "BH"			},
						{ label: "BY",			value: "BY"			},
						{ label: "fdr",			value: "fdr"		}
					]
				}
			}
		}

		Group
		{
			title: qsTr("Cross-validation")
			visible: [6].includes(estimator.currentIndex)
			IntegerField { name: "nFolds"; label: qsTr("nFolds"); min: 3; max: 100000; fieldWidth: 60; defaultValue: 3 }
		}

		VariablesForm
		{
			visible: [7].includes(estimator.currentIndex)
			AvailableVariablesList
			{
				title: qsTr("Variables in network")
				name: "variablesTypeAvailable"
				source: ["variables"]
			}

			AssignedVariablesList { name: "mgmVariableTypeContinuous";	title: qsTr("Continuous Variables");	suggestedColumns: ["scale"]}
			AssignedVariablesList { name: "mgmVariableTypeCategorical";	title: qsTr("Categorical Variables");	suggestedColumns: ["nominal"]}
			AssignedVariablesList { name: "mgmVariableTypeCount";		title: qsTr("Count Variables");			suggestedColumns: ["ordinal"]}
		}
	}

	Section
	{
		title: qsTr("Bootstrap Options")

		Group
		{
			title: qsTr("Settings")
			CheckBox	 { name: "bootstrapOnOff";		label: qsTr("Bootstrap network")	}
			IntegerField { name: "numberOfBootstraps";	label: qsTr("Number of bootstraps"); defaultValue: 0; max: 100000 }

			CheckBox	 { name: "parallelBootstrap";	label: qsTr("Parallel Bootstrap");	checked: false;	visible: false }
		}

		RadioButtonGroup
		{
			name: "BootstrapType"
			title: qsTr("Bootstrap Type")
			Layout.rowSpan: 2
			RadioButton { value: "nonparametric";	label: qsTr("Nonparametric"); checked: true	}
			RadioButton { value: "case";			label: qsTr("Case")							}
			RadioButton { value: "node";			label: qsTr("Node")							}
			RadioButton { value: "parametric";		label: qsTr("Parametric")					}
			RadioButton { value: "person";			label: qsTr("Person")						}
			RadioButton { value: "jackknife";		label: qsTr("Jackknife")					}
		}

		Group
		{
			title: qsTr("Statistics")
			CheckBox { name: "StatisticsEdges";			label: qsTr("Edges");		checked: true }
			CheckBox { name: "StatisticsCentrality";	label: qsTr("Centrality");	checked: true }
		}
	}

	Section
	{
		title: qsTr("Graphical Options")

		InputListView
		{
			id					: networkFactors
			name				: "groupNames"
			title				: qsTr("Group name")
			optionKey			: "group"
			defaultValues		: ["Group 1", "Group 2"]
			placeHolder			: qsTr("New Group")
			minRows				: 2
			preferredWidth		: (2 * form.width) / 5
			enableRowComponents	: manualColors.checked
			rowComponentsTitles	: [qsTr("Group color")]


			rowComponents:
			[
				Component
				{
					DropDown
					{
						name: "groupColors"
						values: [
							{ label: qsTr("red")	, value: "red"		},
							{ label: qsTr("blue")	, value: "blue"		},
							{ label: qsTr("yellow")	, value: "yellow"	},
							{ label: qsTr("green")	, value: "green"	},
							{ label: qsTr("purple")	, value: "purple"	},
							{ label: qsTr("orange") , value: "orange"	}
						]
					}
				}
			]
		}

		AssignedVariablesList
		{
			Layout.fillWidth				: true
			Layout.leftMargin				: 40
			title							: qsTr("Variables in network")
			name							: "variablesForColor"
			source							: ["variables"]
			addAvailableVariablesToAssigned	: true
			draggable						: false
			rowComponentsTitles				: [qsTr("Group")]
			rowComponents:
			[
				Component
				{
					DropDown
					{
						name: "groupAssigned"
						source: ["groupNames"]
					}
				}
			]
		}

		Group
		{
			Layout.columnSpan: 2
			CheckBox	{ name: "manualColors";	label: qsTr("Manual colors");	id: manualColors	}
			DropDown
			{
				enabled: !manualColors.checked
				id: paletteSelector
				name: "nodePalette"
				label: qsTr("Node palette")
				indexDefaultValue: 1
				values: [
					{ label: qsTr("Rainbow"),		value: "rainbow"	},
					{ label: qsTr("Colorblind"),	value: "colorblind"	},
					{ label: qsTr("Pastel"),		value: "pastel"		},
					{ label: qsTr("Gray"),			value: "gray"		},
					{ label: qsTr("R"),				value: "R"			},
					{ label: qsTr("ggplot2"),		value: "ggplot2"	}
				]
			}
			DoubleField	{ name: "nodeSize";		label: qsTr("Node size");		defaultValue: 1; max: 10	}
		}

		Group
		{
			title: qsTr("Edges")
			DoubleField { name: "edgeSize";			label: qsTr("Edge size");			defaultValue: 1 }
			DoubleField { name: "maxEdgeStrength";	label: qsTr("Max edge strength");	defaultValue: 0; max: 10 }
			DoubleField { name: "minEdgeStrength";	label: qsTr("Min edge strength");	defaultValue: 0; max: 10 }
			DoubleField { name: "cut";				label: qsTr("Cut");					defaultValue: 0; max: 10 }
			CheckBox	{ name: "showDetails";		label: qsTr("Show details") }
			CheckBox
			{
								name: "edgeLabels";			label: qsTr("Edge labels");				checked: false
				DoubleField {	name: "edgeLabelCex";		label: qsTr("Edge label size");			min: 0;			max: 10;	defaultValue: 1		}
				DoubleField {	name: "edgeLabelPosition";	label: qsTr("Edge label position");		min: 0;			max: 1;		defaultValue: 0.5	}
			}

			DropDown
			{
				name: "edgeColors"
				label: qsTr("Edge palette")
				indexDefaultValue: 1
				values:
				[
					{ label: qsTr("Classic"),		value: "classic"		},
					{ label: qsTr("Colorblind"),	value: "colorblind"		},
					{ label: qsTr("Gray"),			value: "gray"			},
					{ label: qsTr("Hollywood"),		value: "Hollywood"		},
					{ label: qsTr("Borkulo"),		value: "Borkulo"		},
					{ label: qsTr("TeamFortress"),	value: "TeamFortress"	},
					{ label: qsTr("Reddit"),		value: "Reddit"			},
					{ label: qsTr("Fried"),			value: "Fried"			}
				]
			}
		}

		Group
		{
			title: qsTr("Labels")
			DoubleField { name: "labelSize";	label: qsTr("Label size");		defaultValue: 1; max: 10 }
			CheckBox	{ name: "scaleLabels";	label: qsTr("Scale label size");	checked: true }
			CheckBox
			{
				name: "abbreviateLabels"; label: qsTr("Abbreviate labels to ")
				childrenOnSameRow: true
				IntegerField { name: "abbreviateNoChars"; defaultValue: 4; min: 1; max: 100000 }
			}
		}

		RadioButtonGroup
		{
			name: "showVariableNames";
			title: qsTr("Show Variable Names")
			RadioButton { value: "In nodes";		label: qsTr("In plot");		checked: true	}
			RadioButton { value: "In legend";		label: qsTr("In legend")					}
		}

		RadioButtonGroup
		{
			name: "showMgmVariableType";
			title: qsTr("Show Variable Type")
			visible: [7].includes(estimator.currentIndex)
			RadioButton { value: "mgmNoShow";		label: qsTr("Don't show")						}
			RadioButton { value: "mgmNodeColor";	label: qsTr("Using node color")					}
			RadioButton { value: "mgmNodeShape";	label: qsTr("Using node shape"); checked: true	}
		}

		RadioButtonGroup
		{
			name: "showLegend"
			title: qsTr("Legend")
			RadioButton { value: "No legend";		label: qsTr("No legend")					}
			RadioButton { value: "All plots";		label: qsTr("All plots"); checked: true	}
			RadioButton
			{
				value: "In plot number: "; label: qsTr("In plot number: ")
				childrenOnSameRow: true
				IntegerField { name: "legendNumber"; defaultValue: 1 }
			}
			DoubleField
			{
				name: "legendToPlotRatio"
				label: qsTr("Legend to plot ratio")
				defaultValue: 0.4
				min: 0.001
				max: 4 // not strictly necessary but png crashes if it gets too big
			}
		}

		RadioButtonGroup
		{
			name: "layout"
			title: qsTr("Layout")
			CheckBox { name: "keepLayoutTheSame"; label: qsTr("Do not update layout") }
			RadioButton
			{
				value: "spring"; label: qsTr("Spring"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "repulsion"; label: qsTr("Repulsion"); defaultValue: 1; max: 10 }
			}
			RadioButton { value: "circle";	label: qsTr("Circle")							}
			RadioButton { value: "data";	label: qsTr("Data");	id: dataRatioButton		}
		}

		Group
		{
			title: qsTr("Measures shown in centrality plot")
			enabled: plotCentrality.checked
			CheckBox	{	name: "Betweenness";		label: qsTr("Betweenness");			checked: true	}
			CheckBox	{	name: "Closeness";			label: qsTr("Closeness");			checked: true	}
			CheckBox	{	name: "Degree";				label: qsTr("Betweenness");			checked: true	}
			CheckBox	{	name: "ExpectedInfluence";	label: qsTr("Expected Influence");	checked: true	}
		}

		VariablesForm
		{
			visible: dataRatioButton.checked
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList	{ name: "allXYVariables" }
			AssignedVariablesList	{ name: "layoutX"; title: qsTr("x"); singleVariable: true; suggestedColumns: "nominalText"}
			AssignedVariablesList	{ name: "layoutY"; title: qsTr("y"); singleVariable: true; suggestedColumns: "nominalText"}
		}

		CheckBox
		{
			text: qsTr("Save the layout in the data set")
			name: "addLayoutToData"
			Layout.columnSpan: 2
			ComputedColumnField { name: "computedLayoutX"; text: qsTr("name for x-coordinates"); id: layoutX }
			ComputedColumnField { name: "computedLayoutY"; text: qsTr("name for y-coordinates"); id: layoutY }
			enabled: !dataRatioButton.checked
			visible: !dataRatioButton.checked
			id: layoutCheckbox
			onCheckedChanged:
			{
				if (!layoutCheckbox.checked)
				{
//					The user no longer wants to add the layout to the dataset so we remove it from the dataset if it was there.
					layoutX.value = ""
					layoutY.value = ""
					layoutX.doEditingFinished()
					layoutY.doEditingFinished()
				}
			}
		}
	}
}
