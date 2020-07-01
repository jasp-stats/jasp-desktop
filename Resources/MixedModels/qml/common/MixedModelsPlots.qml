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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3


Section
{
	title:		qsTr("Plots")
	expanded:	false

	VariablesForm
	{
		preferredHeight:	250 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:	"availableModelComponentsPlot"
			title:	qsTr("Model factors")
			source:	[ { name: "fixedEffects", use: "type=ordinal|nominal|nominalText"} ]
		}

		AssignedVariablesList
		{
			name:	"plotsX"
			title:	qsTr("Horizontal axis")
		}

		AssignedVariablesList
		{
			name:	"plotsTrace"
			id:		plotsTrace
			title:	qsTr("Separate lines")
		}

		AssignedVariablesList
		{
			name:	"plotsPanel"
			title:	qsTr("Separate plots")
		}
	}

	VariablesForm
	{
		preferredHeight:	100 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:	"plotsRandom"
			title:	qsTr("Random effects grouping factors")
			source:	"randomVariables"
		}

		AssignedVariablesList
		{
			name:	"plotsAgregatedOver"
			title:	qsTr("Background data show")
			addAvailableVariablesToAssigned: true
		}
	}

	Group
	{
		DropDown
		{
			name:	"plotsCImethod"
			id:		plotsCImethod
			label:	qsTr("Confidence interval method")
			values:
			[
				{ label: qsTr("Model"),			value: "model"},
				{ label: qsTr("None"),			value: "none"},
				{ label: qsTr("Mean"),			value: "mean"},
				{ label: qsTr("Within"),		value: "within"},
				{ label: qsTr("Between"),		value: "between"}
			]
		}

		CIField
		{
			enabled:	plotsCImethod.currentValue != "none"
			name:		"plotsCIwidth"
			label:		qsTr("Confidence interval")
		}
	}

	Group
	{
		title:		qsTr("Distinguish factor levels")
		columns:	4

		CheckBox
		{
			name:		"plotsMappingColor"
			label:		qsTr("Color")
			checked:	false
		}

		CheckBox
		{
			name:		"plotsMappingShape"
			label:		qsTr("Shape")
			checked:	true
		}

		CheckBox
		{
			name:		"plotsMappingLineType"
			label:		qsTr("Linetype")
			checked:	true
		}

		CheckBox
		{
			name:		"plotsMappingFill"
			label:		qsTr("Fill")
			checked:	false
			enabled:	plotsGeom.currentValue != "geom_jitter"
			onEnabledChanged:  checked = false
		}
	}

	Group
	{
		columns:	1

		DropDown
		{
			name:	"plotsGeom"
			label:	qsTr("Background geom")
			id:		plotsGeom
			values:
			[
				{ label: qsTr("Jitter"),			value: "geom_jitter"},
			//	{ label: qsTr("Beeswarm"),			value: "geom_beeswarm"}, # enable once the package loading is changed
				{ label: qsTr("Violin"),			value: "geom_violin"},
				{ label: qsTr("Boxplot"),			value: "geom_boxplot"},
				{ label: qsTr("Boxjitter"),			value: "geom_boxjitter"},
				{ label: qsTr("Count"),				value: "geom_count"}
			]
		}

		DoubleField
		{
			name:			"plotAlpha"
			label:			qsTr("Transparency")
			defaultValue:	.7
			min:			0
			max: 			1
			inclusive:		JASP.None
		}

		DoubleField
		{
			visible:		plotsGeom.currentValue == "geom_jitter" || plotsGeom.currentValue == "geom_boxjitter"
			name:			"plotJitterWidth"
			label:			qsTr("Jitter width")
			defaultValue:	0.1
			min:			0
		}

		DoubleField
		{
			visible:		plotsGeom.currentValue == "geom_jitter" | plotsGeom.currentValue == "geom_boxjitter"
			name:			"plotJitterHeight"
			label:			qsTr("Jitter height")
			defaultValue:	0
			min:			0
		}

		DoubleField
		{
			visible:		plotsGeom.currentValue == "geom_violin" | plotsGeom.currentValue == "geom_boxplot" | plotsGeom.currentValue == "geom_boxjitter"
			name:			"plotGeomWidth"
			label:			qsTr("Geom width")
			defaultValue:	1
			min:			0
		}

		DoubleField
		{
			visible:		plotsTrace.count != 0
			name:			"plotDodge"
			label:			qsTr("Dodge")
			defaultValue:	0.3
			min:			0
		}
	}

	Group
	{
		columns:	1

		DropDown
		{
			name:	"plotsTheme"
			id:		plotsTheme
			label:	qsTr("Theme")
			values:
			[
				{ label: "JASP",				value: "JASP"},
				{ label: qsTr("Black White"),	value: "theme_bw"},
				{ label: qsTr("Light"),			value: "theme_light"},
				{ label: qsTr("Minimal"),		value: "theme_minimal"},
				{ label: "APA",					value: "jtools::theme_apa"},
				{ label: "pubr",				value: "ggpubr::theme_pubr"}
			]
		}

		DropDown
		{
			name:	"plotLegendPosition"
			label:	qsTr("Legend position")
			values:
			[
				{ label: qsTr("None"),			value: "none"},
				{ label: qsTr("Bottom"),		value: "bottom"},
				{ label: qsTr("Right"),			value: "right"},
				{ label: qsTr("Top"),			value: "top"},
				{ label: qsTr("Left"), 			value: "left"}
			]
		}

		DropDown
		{
			name:	"plotsBackgroundColor"
			label:	qsTr("Color background data")
			enabled:plotsGeom.currentValue != "geom_jitter"
			values:
			[
				{ label: qsTr("Dark grey"),			value: "darkgrey"},
				{ label: qsTr("None"),				value: "none"},
				{ label: qsTr("Black"),				value: "black"},
				{ label: qsTr("Light grey"),		value: "lightgrey"},
				{ label: qsTr("Blue"),				value: "blue"},
				{ label: qsTr("Red"),				value: "red"},
				{ label: qsTr("Violet"),			value: "violet"}
			]
		}

		DoubleField
		{
			enabled:		plotsTheme.currentValue != "JASP"
			name:			"plotRelativeSizeText"
			label:			qsTr("Relative size text")
			defaultValue:	1.5
			min:			0
		}

		DoubleField
		{
			name:			"plotRelativeSize"
			label:			qsTr("Relative size foreground data")
			defaultValue:	1
			min:			0
		}

		CheckBox
		{
			name:	"plotsEstimatesTable"
			label:	qsTr("Estimates table")
		}
	}
}
