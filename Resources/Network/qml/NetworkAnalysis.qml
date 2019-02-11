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
import JASP.Theme 1.0

Form
{
	usesJaspResults: false
	
	TextField	 { name: "layoutX";					value: "";			visible: false }
	TextField	 { name: "layoutY";					value: "";			visible: false }
	CheckBox	 { name: "parallelBootstrap";		checked: false;		visible: false }
	IntegerField { name: "plotHeightBootstrapPlot";	defaultValue: 320;	visible: false }
	IntegerField { name: "plotHeightCentrality";	defaultValue: 320;	visible: false }
	IntegerField { name: "plotHeightClustering";	defaultValue: 320;	visible: false }
	IntegerField { name: "plotHeightNetwork";		defaultValue: 320;	visible: false }
	IntegerField { name: "plotWidthBootstrapPlot";	defaultValue: 480;	visible: false }
	IntegerField { name: "plotWidthCentrality";		defaultValue: 480;	visible: false }
	IntegerField { name: "plotWidthClustering";		defaultValue: 480;	visible: false }
	IntegerField { name: "plotWidthNetwork";		defaultValue: 480;	visible: false }
	CheckBox	 { name: "tableFitMeasures";		checked: false;		visible: false }	
	
	VariablesForm 
	{
		AssignedVariablesList { name: "variables";			title: qsTr("Dependent Variables") }
		AssignedVariablesList { name: "groupingVariable";	title: qsTr("Split"); singleItem: true; allowedColumns: ["ordinal", "nominal"] }
	}
	
	DropDown
	{
		id: estimator
		name: "estimator"
		text: qsTr("Estimator")
		Layout.columnSpan: 2
		values: ["EBICglasso", "cor", "pcor", "IsingFit", "IsingSampler", "huge", "adalasso", "mgm"]
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox { name: "plotNetwork";		text: qsTr("Network plot")		}
		CheckBox { name: "plotCentrality";	text: qsTr("Centrality plot")	}
		CheckBox { name: "plotClustering";	text: qsTr("Clustering plot")	}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { name: "tableCentrality";			text: qsTr("Centrality tabel")					}
		CheckBox { name: "tableClustering";			text: qsTr("Clustering tabel")					}
		CheckBox { name: "tableWeightsMatrix";		text: qsTr("Weights matrix")					}
		CheckBox
		{
			name: "tableLayout"; text: qsTr("Layout matrix")
			CheckBox { name: "tableLayoutValuesOnly"; text: qsTr("Show variable names") }
		}
	}

	ExpanderButton 
	{
		title: qsTr("Analysis Options - ") + estimator.currentText

		RadioButtonGroup
		{
			name: "correlationMethod"
			title: qsTr("Correlation method")
			visible: [0, 1, 2].includes(estimator.currentIndex)
			RadioButton { value: "auto";	text: qsTr("Auto");	checked: true	}
			RadioButton { value: "cor";		text: qsTr("Cor")					}
			RadioButton { value: "cov";		text: qsTr("Cov")					}
			RadioButton { value: "npn";		text: qsTr("Npn")					}
		}

		RadioButtonGroup
		{
			name: "normalizeCentrality"
			title: qsTr("Centrality measures")
			visible: estimator.currentIndex === 0
			RadioButton { value: "normalized";	text: qsTr("Normalized"); checked: true	}
			RadioButton { value: "relative" ;	text: qsTr("Relative")					}
			RadioButton { value: "raw";			text: qsTr("Raw")						}
		}

		Group
		{
			title: qsTr("Network")
			visible: estimator.currentIndex === 0
			CheckBox { name: "weightedNetwork"; text: qsTr("Weighted");	checked: true }
			CheckBox { name: "signedNetwork";	text: qsTr("Signed");	checked: true }
		}

		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing values")
			visible: [0, 1, 2].includes(estimator.currentIndex)
			RadioButton { value: "pairwise";	text: qsTr("Exclude pairwise"); checked: true	}
			RadioButton { value: "listwise";	text: qsTr("Exclude listwise")					}
		}

		RadioButtonGroup
		{
			name: "sampleSize"
			title: qsTr("Sample size")
			visible: estimator.currentIndex === 0
			RadioButton { value: "maximum";	text: qsTr("Maximum"); checked: true	}
			RadioButton { value: "minimim";	text: qsTr("Minimum")					}
		}

		RadioButtonGroup
		{
			name: "isingEstimator"
			title: qsTr("Ising Estimator")
			visible: estimator.currentIndex === 4
			RadioButton { value: "pseudoLikelihood";		text: qsTr("Pseudo-likelihood"); checked: true	}
			RadioButton { value: "univariateRegressions";	text: qsTr("Univariate regressions")			}
			RadioButton { value: "bivariateRegressions";	text: qsTr("Bivariate regressions")				}
			RadioButton { value: "logLinear";				text: qsTr("Loglinear")							}
		}

		RadioButtonGroup
		{
			name: "criterion"
			title: qsTr("Criterion")
			visible: [5, 7].includes(estimator.currentIndex)
			RadioButton { value: "ebic";	text: qsTr("EBIC"); checked: true	}
			RadioButton { value: "ric";	text: qsTr("RIC")						}
			RadioButton { value: "stars";	text: qsTr("STARS")					}
			RadioButton { value: "cv";	text: qsTr("CV")						}
		}

		RadioButtonGroup
		{
			name: "rule"
			title: qsTr("Rule")
			visible: [3, 7].includes(estimator.currentIndex)
			RadioButton { value: "and";	text: qsTr("AND"); checked: true	}
			RadioButton { value: "or";	text: qsTr("OR")					}
		}

		RadioButtonGroup
		{
			name: "split"
			title: qsTr("Split")
			visible: [3, 4].includes(estimator.currentIndex)
			RadioButton { value: "median";	text: qsTr("Median"); checked: true	}
			RadioButton { value: "mean";	text: qsTr("Mean")					}
		}

		Group
		{
			title: qsTr("Tuning parameters")
			visible: [0, 3, 5, 7].includes(estimator.currentIndex)
			DoubleField { name: "tuningParameter"; text: qsTr("value"); defaultValue: 0.5; max: 1 }
		}

		RadioButtonGroup
		{
			name: "thresholdBox"
			title: qsTr("Threshold")
			visible: [1, 2].includes(estimator.currentIndex)
			RadioButton
			{
				value: "value";	text: qsTr("Value"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "thresholdValue"; defaultValue: 0; max: 1000000000 }
			}
			RadioButton
			{
				value: "method"; text: qsTr("Method")
				childrenOnSameRow: true
				DropDown
				{
					name: "thresholdMethod"
					model: ListModel
					{
						ListElement { title: "Significant"; value: "sig" }
						ListElement { title: "Bonferroni"; value: "bonferroni" }
						ListElement { title: "locfdr"; value: "locfdr" }
						ListElement { title: "Holm"; value: "holm" }
						ListElement { title: "Hochberg"; value: "hochberg" }
						ListElement { title: "Hommel"; value: "hommel" }
						ListElement { title: "BH"; value: "BH" }
						ListElement { title: "BY"; value: "BY" }
						ListElement { title: "fdr"; value: "fdr" }
					}
				}
			}
		}

		Group
		{
			title: qsTr("Cross-validation")
			visible: [6].includes(estimator.currentIndex)
			IntegerField { name: "nFolds"; text: qsTr("nFolds"); min: 3; max: 100000; fieldWidth: 60 }
		}

		VariablesForm
		{
			visible: [7].includes(estimator.currentIndex)
			height: 150
			availableVariablesList.name: "variablesTypeAvailable"
			AssignedVariablesList { name: "mgmVariableType";	title: qsTr("Variable Type"); singleItem: true; allowedColumns: ["nominal"] }
		}
	}

	ExpanderButton 
	{
		title: qsTr("Bootstrap Options")
		
		Group
		{
			title: qsTr("Settings")
			CheckBox	 { name: "bootstrapOnOff";		text: qsTr("Bootstrap network")	}
			IntegerField { name: "numberOfBootstraps";	text: qsTr("Number of bootstraps"); defaultValue: 0; max: 100000 }
		}

		RadioButtonGroup
		{
			name: "BootstrapType"
			title: qsTr("Bootstrap type")
			Layout.rowSpan: 2
			RadioButton { value: "nonparametric";	text: qsTr("Nonparametic");	 checked: true	}
			RadioButton { value: "case";			text: qsTr("Case")							}
			RadioButton { value: "node";			text: qsTr("Node")							}
			RadioButton { value: "parametric";		text: qsTr("Parametric")					}
			RadioButton { value: "person";			text: qsTr("Person")						}
			RadioButton { value: "jackknife";		text: qsTr("Jackknife")						}
		}

		Group
		{
			title: qsTr("Statistics")
			CheckBox { name: "StatisticsEdges";			text: qsTr("Edges");		checked: true }
			CheckBox { name: "StatisticsCentrality";	text: qsTr("Centrality");	checked: true }
		}
	}
	
	ExpanderButton 
	{
		title: qsTr("Graphical Options")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "variablesForColor"; title: qsTr("Nodes") }
			AssignedVariablesList  { name: "colorNodesBy";		title: qsTr("Color nodes by"); singleItem: true }
		}
		
		Group
		{
			Layout.columnSpan: 2
			DoubleField { name: "nodeSize"; text: qsTr("Node size"); defaultValue: 1; max: 10 }
			DropDown
			{
				name: "nodeColors"
				text: qsTr("Node palette")
				indexDefaultValue: 1
				model: ListModel {
					ListElement { title: qsTr("Rainbow");		value: "rainbow"	}
					ListElement { title: qsTr("Colorblind");	value: "colorblind" }
					ListElement { title: qsTr("Pastel");		value: "pastel"		}
					ListElement { title: qsTr("Gray");			value: "gray"		}
					ListElement { title: qsTr("R");				value: "R"			}
					ListElement { title: qsTr("ggplot2");		value: "ggplot2"	}
				}
			}
		}

		Group
		{
			title: qsTr("Edges")
			DoubleField { name: "edgeSize";			text: qsTr("Edge size");			defaultValue: 1 }
			DoubleField { name: "maxEdgeStrength";	text: qsTr("Max edge strength");	defaultValue: 0; max: 10 }
			DoubleField { name: "minEdgeStrength";	text: qsTr("Min edge strength");	defaultValue: 0; max: 10 }
			DoubleField { name: "cut";				text: qsTr("Cut");					defaultValue: 0; max: 10 }
			CheckBox	{ name: "showDetails";		text: qsTr("Show details") }
			DropDown
			{
				name: "edgeColors"
				text: qsTr("Edge palette")
				indexDefaultValue: 1
				model: ListModel
				{
					ListElement { title: qsTr("Classic");		value: "classic"	}
					ListElement { title: qsTr("Colorblind");	value: "colorblind" }
					ListElement { title: qsTr("Gray");			value: "gray"		}
					ListElement { title: qsTr("Hollywood");		value: "Hollywood"	}
					ListElement { title: qsTr("Borkulo");		value: "Borkulo"	}
					ListElement { title: qsTr("TeamFortress");	value: "TeamFortress" }
					ListElement { title: qsTr("Reddit");		value: "Reddit"		}
					ListElement { title: qsTr("Fried");			value: "Fried"		}
				}

			}
		}

		Group
		{
			title: qsTr("Labels")
			DoubleField { name: "labelSize";	text: qsTr("Label size");		defaultValue: 1; max: 10 }
			CheckBox	{ name: "scaleLabels";	text: qsTr("Scale label size");	checked: true }
			CheckBox
			{
				name: "abbreviateLabels"; text: qsTr("Abbreviate labels to ")
				childrenOnSameRow: true
				IntegerField { name: "abbreviateNoChars"; defaultValue: 4; max: 100000 }
			}

		}

		RadioButtonGroup
		{
			name: "graphSize";
			title: qsTr("Network size")
			RadioButton { value: "graphSizeFixed";	text: qsTr("Fixed ratio"); checked: true	}
			RadioButton { value: "graphSizeFree";	text: qsTr("Free")							}
		}

		RadioButtonGroup
		{
			name: "showVariableNames";
			title: qsTr("Show variable names")
			RadioButton { value: "In nodes";		text: qsTr("In nodes");	 checked: true	}
			RadioButton { value: "In legend";		text: qsTr("In legend")					}
		}

		RadioButtonGroup
		{
			name: "showMgmVariableType";
			title: qsTr("Show variable type")
			visible: [7].includes(estimator.currentIndex)
			RadioButton { value: "mgmNoShow";		text: qsTr("Don't show")						}
			RadioButton { value: "mgmNodeColor";	text: qsTr("Using node color")					}
			RadioButton { value: "mgmNodeShape";	text: qsTr("Using node shape"); checked: true	}
		}

		RadioButtonGroup
		{
			name: "showLegend"
			title: qsTr("Legend")
			RadioButton { value: "No legend";		text: qsTr("No legend")					}
			RadioButton { value: "All plots";		text: qsTr("All plots"); checked: true	}
			RadioButton
			{
				value: "In plot number: "; text: qsTr("In plot number: ")
				childrenOnSameRow: true
				IntegerField { name: "legendNumber"; defaultValue: 1 }
			}
		}

		RadioButtonGroup
		{
			name: "layout"
			title: qsTr("Layout")
			CheckBox { name: "keepLayoutTheSame"; text: qsTr("Do not update layout") }
			RadioButton
			{
				value: "spring"; text: qsTr("Spring"); checked: true
				childrenOnSameRow: true
				DoubleField { name: "repulsion"; text: qsTr("Repulsion"); defaultValue: 1; max: 10 }
			}
			RadioButton { value: "circle";	text: qsTr("Circle")	}
			RadioButton { value: "data";	text: qsTr("Data")		}
		}
	}
}
