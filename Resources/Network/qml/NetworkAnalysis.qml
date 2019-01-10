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
	id: form
	usesJaspResults: false

	TextField	 { name: "isingEstimator";			value: "pseudoLikelihood"; visible: false }
	TextField	 { name: "layoutX";					value: "";			visible: false }
	TextField	 { name: "layoutY";					value: "";			visible: false }
	TextField	 { name: "mgmVariableType";			value: "";			visible: false }
	IntegerField { name: "nFolds";					defaultValue: 3;	visible: false }
	CheckBox	 { name: "parallelBootstrap";		checked: false;		visible: false }
	IntegerField { name: "plotHeightBootstrapPlot";	defaultValue: 320;	visible: false }
	IntegerField { name: "plotHeightCentrality";	defaultValue: 320;	visible: false }
	IntegerField { name: "plotHeightClustering";	defaultValue: 320;	visible: false }
	IntegerField { name: "plotHeightNetwork";		defaultValue: 320;	visible: false }
	IntegerField { name: "plotWidthBootstrapPlot";	defaultValue: 480;	visible: false }
	IntegerField { name: "plotWidthCentrality";		defaultValue: 480;	visible: false }
	IntegerField { name: "plotWidthClustering";		defaultValue: 480;	visible: false }
	IntegerField { name: "plotWidthNetwork";		defaultValue: 480;	visible: false }
	TextField	 { name: "rule";					value: "and";		visible: false }
	TextField	 { name: "showMgmVariableType";		value: "mgmNodeShape"; visible: false }
	TextField	 { name: "split";					value: "mean";		visible: false }
	CheckBox	 { name: "tableFitMeasures";		checked: false;		visible: false }
	TextField	 { name: "thresholdBox";			value: "value";		visible: false }
	TextField	 { name: "thresholdString";			value: "sig";		visible: false }
	DoubleField	 { name: "thresholdValue";			defaultValue: 0;	visible: false }

	
	VariablesForm 
	{
		defaultAssignedVariablesList.title: qsTr("Dependent Variables")
		AssignedVariablesList 
		{
			name: "groupingVariable"
			title: qsTr("Split")
			singleItem: true
			allowedColumns: ["ordinal", "nominal"]
		}
	}

	GridLayout
	{
		ColumnLayout
		{
			ComboBox
			{
				text: qsTr("Estimator")
				name: "estimator"
				model: ["EBICglasso", "cor", "pcor", "IsingFit", "IsingSampler", "huge", "adalasso", "mgm"]
			}

			GroupBox
			{
				title: qsTr("Plots")
				CheckBox { text: qsTr("Network plot");		name: "plotNetwork" }
				CheckBox { text: qsTr("Centrality plot");	name: "plotCentrality" }
				CheckBox { text: qsTr("Clustering plot");	name: "plotClustering" }
			}
		}

		GroupBox
		{
			title: qsTr("Tables")
			CheckBox { text: qsTr("Centrality tabel");		name: "tableCentrality" }
			CheckBox { text: qsTr("Clustering tabel");		name: "tableClustering" }
			CheckBox { text: qsTr("Weights matrix");		name: "tableWeightsMatrix" }
			CheckBox { text: qsTr("Layout matrix");			name: "tableLayout"; id: tableLayout }
			CheckBox { text: qsTr("Show variable names");	name: "tableLayoutValuesOnly"; enabled: tableLayout.checked; indent: true }
		}
	}

	
	ExpanderButton 
	{
		title: qsTr("Analysis Options")
		GridLayout
		{
			ButtonGroup
			{
				title: qsTr("Correlation method")
				name: "correlationMethod"
				RadioButton { text: qsTr("Auto");	name: "auto"; checked: true }
				RadioButton { text: qsTr("Cor");	name: "cor" }
				RadioButton { text: qsTr("Cov");	name: "cov" }
				RadioButton { text: qsTr("Npn");	name: "npn" }
			}

			ButtonGroup
			{
				title: qsTr("Centrality measures")
				name: "normalizeCentrality"
				RadioButton { text: qsTr("Normalized"); name: "normalized"; checked: true }
				RadioButton { text: qsTr("Relative");	name: "relative" }
				RadioButton { text: qsTr("Raw");		name: "raw" }
			}

			GroupBox
			{
				title: qsTr("Network")
				CheckBox { text: qsTr("Weighted");	name: "weightedNetwork"; checked: true }
				CheckBox { text: qsTr("Signed");	name: "signedNetwork";	 checked: true }
			}

			ButtonGroup
			{
				title: qsTr("Missing values")
				name: "missingValues"
				RadioButton { text: qsTr("Exclude pairwise"); name: "pairwise"; checked: true }
				RadioButton { text: qsTr("Exclude listwise"); name: "listwise" }
			}

			ButtonGroup
			{
				title: qsTr("Sample size")
				name: "sampleSize"
				RadioButton { text: qsTr("Maximum"); name: "maximum"; checked: true }
				RadioButton { text: qsTr("Minimum"); name: "minimim" }
			}

			GroupBox
			{ 
				title: qsTr("Tuning parameters")
				DoubleField { text: qsTr("value"); name: "tuningParameter"; defaultValue: 0.5; doubleValidator.top: 1 }
			}
		}
	}
	
	ExpanderButton 
	{
		text: qsTr("Bootstrap Options")
		
		GridLayout 
		{
			ColumnLayout
			{
				GroupBox
				{
					title: qsTr("Settings")
					CheckBox	 { text: qsTr("Bootstrap network");		name: "bootstrapOnOff" }
					IntegerField { text: qsTr("Number of bootstraps");	name: "numberOfBootstraps"; defaultValue: 0; intValidator.top: 100000}
				}

				GroupBox
				{
					title: qsTr("Statistics")
					CheckBox { text: qsTr("Edges");			name: "StatisticsEdges";  checked: true }
					CheckBox { text: qsTr("Centrality");	name: "StatisticsCentrality"; checked: true }
				}
			}
			
			ButtonGroup
			{
				title: qsTr("Bootstrap type")
				name: "BootstrapType"
				RadioButton { text: qsTr("Nonparametic");	name: "nonparametric"; checked: true }
				RadioButton { text: qsTr("Case");			name: "case" }
				RadioButton { text: qsTr("Node");			name: "node" }
				RadioButton { text: qsTr("Parametric");		name: "parametric" }
				RadioButton { text: qsTr("Person");			name: "person" }
				RadioButton { text: qsTr("Jackknife");		name: "jackknife" }
			}
		}
	}
	
	ExpanderButton 
	{
		text: qsTr("Graphical Options")

		VariablesForm
		{
			height: 200
			availableVariablesList
			{
				name: "variablesForColor"
				title: qsTr("Nodes")
			}
			defaultAssignedVariablesList
			{
				title: qsTr("Color nodes by")
				name: "colorNodesBy"
				singleItem: true
			}
		}

		GridLayout 
		{
			GroupBox
			{
				Layout.columnSpan: 2
				DoubleField { text: qsTr("Node size"); name: "nodeSize"; defaultValue: 1; doubleValidator.top: 10 }
				ComboBox
				{
					text: qsTr("Node palette")
					name: "nodeColors"
					currentIndex: 1
					model: ListModel {
						ListElement { key: qsTr("Rainbow");		value: "rainbow"	}
						ListElement { key: qsTr("Colorblind");	value: "colorblind" }
						ListElement { key: qsTr("Pastel");		value: "pastel"		}
						ListElement { key: qsTr("Gray");		value: "gray"		}
						ListElement { key: qsTr("R");			value: "R"			}
						ListElement { key: qsTr("ggplot2");		value: "ggplot2"	}
					}
				}
			}

			GroupBox
			{
				title: qsTr("Edges")
				DoubleField { text: qsTr("Edge size");			name: "edgeSize";		 defaultValue: 1 }
				DoubleField { text: qsTr("Max edge strength");	name: "maxEdgeStrength"; defaultValue: 0; doubleValidator.top: 10 }
				DoubleField { text: qsTr("Min edge strength");	name: "minEdgeStrength"; defaultValue: 0; doubleValidator.top: 10 }
				DoubleField { text: qsTr("Cut");				name: "cut";			 defaultValue: 0; doubleValidator.top: 10 }
				CheckBox	{ text: qsTr("Show details");		name: "showDetails" }
				ComboBox
				{
					text: qsTr("Edge palette")
					name: "edgeColors"
					currentIndex: 1
					model: ListModel {
						ListElement { key: qsTr("Classic");		value: "classic"	}
						ListElement { key: qsTr("Colorblind");	value: "colorblind" }
						ListElement { key: qsTr("Gray");		value: "gray"		}
						ListElement { key: qsTr("Hollywood");	value: "Hollywood"	}
						ListElement { key: qsTr("Borkulo");		value: "Borkulo"	}
						ListElement { key: qsTr("TeamFortress"); value: "TeamFortress" }
						ListElement { key: qsTr("Reddit");		value: "Reddit"		}
						ListElement { key: qsTr("Fried");		value: "Fried"		}
					}

				}
			}

			GroupBox
			{
				title: qsTr("Labels")
				DoubleField { text: qsTr("Label size");			name: "labelSize";		defaultValue: 1; doubleValidator.top: 10 }
				CheckBox	{ text: qsTr("Scale label size");	name: "scaleLabels"; checked: true }
				Row
				{
					CheckBox { text: qsTr("Abbreviate labels to "); name: "abbreviateLabels"; id: abbreviateLabels }
					IntegerField { name: "abbreviateNoChars"; enabled: abbreviateLabels.checked; defaultValue: 4; intValidator.top: 100000 }
				}

			}

			ButtonGroup 
			{ 
				name: "graphSize";
				title: qsTr("Network size")
				RadioButton { text: qsTr("Fixed ratio"); name: "graphSizeFixed"; checked: true	}
				RadioButton { text: qsTr("Free");		name: "graphSizeFree"					}
			}
			
			ButtonGroup
			{
				name: "showVariableNames";
				title: qsTr("Show variable names")
				RadioButton { text: qsTr("In nodes");	name: "In nodes"; checked: true	}
				RadioButton { text: qsTr("In legend");	name: "In legend"				}
			}

			ButtonGroup
			{
				name: "showLegend"
				title: qsTr("Legend")
				RadioButton { text: qsTr("No legend"); name: "No legend" }
				RadioButton { text: qsTr("All plots"); name: "All plots"; checked: true }
				Row
				{
					RadioButton  { text: qsTr("In plot number: "); name: "In plot number: " }
					IntegerField { name: "legendNumber"; defaultValue: 1 }
				}
			}

			ButtonGroup
			{
				name: "layout"
				title: qsTr("Layout")
				CheckBox { text: qsTr("Do not update layout"); name: "keepLayoutTheSame" }
				Row
				{
					RadioButton { text: qsTr("Spring"); name: "spring"; checked: true; id: springLayout }
					DoubleField { text: qsTr("Repulsion"); name: "repulsion"; defaultValue: 1; doubleValidator.top: 10; enabled: springLayout.checked }
				}
				RadioButton { text: qsTr("Circle"); name: "circle" }
				RadioButton { text: qsTr("Data"); name: "data" }
			}
		}
	}
}
