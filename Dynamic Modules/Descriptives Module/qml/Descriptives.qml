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

// All Analysis forms must be built with the Form QML item
Form
{
	usesJaspResults: true

	VariablesForm
	{
		AssignedVariablesList { name: "variables";	title: qsTr("Variables") }
		AssignedVariablesList { name: "splitby";	title: qsTr("Split"); singleItem: true; allowedColumns: ["ordinal", "nominal"] }
	}

	CheckBox { name: "frequencyTables"; text: qsTr("Frequency tables (nominal and ordinal variables)") }

	ExpanderButton
	{
		title: qsTr("Plots")

		GroupBox
		{
			CheckBox { name: "plotVariables";			text: qsTr("Distribution plots"); id: plotVariables							}
			CheckBox { name: "distPlotDensity";			text: qsTr("Display density"); enabled: plotVariables.checked; indent: true	}
			CheckBox { name: "plotCorrelationMatrix";	text: qsTr("Correlation plots")												}
			CheckBox { name: "splitPlots";				text: qsTr("Boxplots");	id: splitPlots										}

			GroupBox
			{
				indent: true
				enabled: splitPlots.checked

				CheckBox { name: "splitPlotOutlierLabel";	text: qsTr("Label Outliers")	}
				CheckBox { name: "splitPlotColour";			text: qsTr("Color")				}

				GroupBox
				{
					indent: true;
					CheckBox { name: "splitPlotBoxplot";	text: qsTr("Boxplot Element"); checked: true	}
					CheckBox { name: "splitPlotViolin";		text: qsTr("Violin Element")					}
					CheckBox { name: "splitPlotJitter";		text: qsTr("Jitter Element")					}
				}
			}
		}
	}

	ExpanderButton
	{
		title: qsTr("Statistics")

		GridLayout
		{
			GroupBox
			{
				title: qsTr("Percentile Values")

				CheckBox { name: "percentileValuesQuartiles";	text: qsTr("Quartiles") }
				GridLayout
				{
					rowSpacing: Theme.rowGroupSpacing
					columnSpacing: 1

					CheckBox { name: "percentileValuesEqualGroups"; text: qsTr("Cut points for: "); id: percentileValuesEqualGroups }
					IntegerField
					{
						name: "percentileValuesEqualGroupsNo"
						intValidator { bottom: 1; top: 1000 }
						defaultValue: 4
						enabled: percentileValuesEqualGroups.checked
						afterLabel.text: qsTr(" equal groups")
					}
					CheckBox { name: "percentileValuesPercentiles"; text: qsTr("Percentiles:"); id: percentileValuesPercentiles }
					TextField
					{
						inputType: "integerArray"
						name: "percentileValuesPercentilesPercentiles"
						fieldWidth: 60
						enabled: percentileValuesPercentiles.checked
					}
				}
			}

			GroupBox
			{
				title: qsTr("Central Tendency")
				CheckBox { name: "mean";			text: qsTr("Mean");		checked: true	}
				CheckBox { name: "median";			text: qsTr("Median")					}
				CheckBox { name: "mode";			text: qsTr("Mode");						}
				CheckBox { name: "sum";				text: qsTr("Sum");						}
			}

			GroupBox
			{
				title: qsTr("Dispersion")
				CheckBox { name: "standardDeviation";	text: qsTr("Std.deviation"); checked: true	}
				CheckBox { name: "minimum";				text: qsTr("Minimum");		checked: true	}
				CheckBox { name: "maximum";				text: qsTr("Maximum");		checked: true	}
				CheckBox { name: "variance";			text: qsTr("Variance")						}
				CheckBox { name: "range";				text: qsTr("Range")							}
				CheckBox { name: "standardErrorMean";	text: qsTr("S. E. mean")					}
			}
			GroupBox
			{
				title: qsTr("Distribution")
				CheckBox { name: "skewness";			text: qsTr("Skewness")						}
				CheckBox { name: "kurtosis";			text: qsTr("Kurtosis")						}
			}
		}

		CheckBox { name: "statisticsValuesAreGroupMidpoints"; text: qsTr("Values are group midpoints"); debug: true }
	}

	ExpanderButton
	{
		title: qsTr("Charts")
		debug: true
		GridLayout
		{
			RadioButtonGroup
			{
				name: "chartType";
				title: qsTr("Chart Type")
				RadioButton { value: "_1noCharts";		text: qsTr("None")			}
				RadioButton { value: "_2barCharts";		text: qsTr("Bar charts")	}
				RadioButton { value: "_3pieCharts";		text: qsTr("Pie Charts")	}
				RadioButton { value: "_4histograms";	text: qsTr("Histograms")	}
			}

			RadioButtonGroup
			{
				name: "chartValues"
				title: qsTr("Chart Values")
				RadioButton { value: "_1frequencies";	text: qsTr("Frequencies")	}
				RadioButton { value: "_2percentages";	text: qsTr("Percentages")	}
			}
		}
	}
}
