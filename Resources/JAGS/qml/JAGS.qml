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

import QtQuick.Layouts	1.3
import QtQuick			2.8

import JASP.Widgets		1.0
import JASP.Controls	1.0

Form
{
	columns: 1
	TextArea
	{
		id:			jagsModel
		title:		qsTr("Enter JAGS model below")
		name:		"model"
		textType:	"JAGSmodel"
		text:		"model{\n\n}"
	}

	VariablesForm
	{
		visible: !monitorAllParameters.checked
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList
		{
			name: "parametersList";
			title: qsTr("Parameters in model")
			source: [{ name: "model", discard: { name: "userData", use: "Parameter"}}]
		}
		AssignedVariablesList   { name: "monitoredParametersList";   title: qsTr("Monitor these parameters"); }
	}

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList
		{
			id:		monitoredParametersList2
			name:	"monitoredParametersList2"
			title:	monitorAllParameters.checked ? qsTr("Parameters in model") : qsTr("Monitored parameters")
			source:	monitorAllParameters.checked ? [{ name: "model", discard: { name: "userData", use: "Parameter"}}] : ["monitoredParametersList"]
		}
		AssignedVariablesList   { name: "parametersShown";		title: qsTr("Show results for these parameters")}
	}

	Section
	{
		title: qsTr("Plots")
		Group
		{
			DropDown
			{
				name: "colorScheme"
				indexDefaultValue: 0
				label: qsTr("Color scheme for plots:")
				values:
					[
					{ label: qsTr("Colorblind"),		value: "colorblind"		},
					{ label: qsTr("Colorblind Alt."),	value: "colorblind2"	},
					{ label: qsTr("Viridis"),			value: "viridis"		},
					{ label: qsTr("Blue"),				value: "blue"			},
					{ label: qsTr("Gray"),				value: "gray"			}
				]
			}
			CheckBox { name: "aggregateChains";	label: qsTr("Aggregate chains for densities and histograms");	checked:true	}
			CheckBox { name: "showLegend";		label: qsTr("Show legends");									checked:true	}
			CheckBox { name: "plotDensity";		label: qsTr("Density")															}
			CheckBox { name: "plotHistogram";	label: qsTr("Histogram")														}
			CheckBox { name: "plotTrace";		label: qsTr("Trace");															}
		}
		Group
		{
			CheckBox { label: qsTr("Autocorrelation");    name: "plotAutoCor"; id: autoCorrelation
				IntegerField
				{
					name: "noLags"
					label: qsTr("No. lags")
					defaultValue: 20
					min: 1
					max: 100
				}
				RadioButtonGroup
				{
					name: "acfType"
					title: qsTr("Type")
					RadioButton { value: "acfLines";  label: qsTr("line"); checked:true	}
					RadioButton { value: "acfBars";   label: qsTr("bar")				}
				}
			}
			CheckBox { label: qsTr("Bivariate scatter");  name: "plotBivarHex"; id: bivariateScatter
				RadioButtonGroup
				{
					name: "bivariateScatterDiagType"
					title: qsTr("Diagonal plot type")
					RadioButton { value: "dens";  label: qsTr("Density"); checked:true	}
					RadioButton { value: "hist";  label: qsTr("Histogram")				}
				}
				RadioButtonGroup
				{
					name: "bivariateScatterOffDiagType"
					title: qsTr("Off-diagonal plot type")
					RadioButton { value: "hex";     label: qsTr("Hexagonal"); checked:true	}
					RadioButton { value: "scatter"; label: qsTr("Contour")					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Initial Values")
		JagsTableView
		{
			name		:	"initialValues"
			tableType	:	"initialValues"
			source		:	[{ name: "model", discard: { name: "userData", use: "Parameter"}}]
		}
	}

	Section
	{
		title: qsTr("Observed Values")
		JagsTableView
		{
			name		:	"userData"
			tableType	:	"userDataInput"
		}
	}

	Section
	{
		title: qsTr("Advanced")
		columns: 2
		Group
		{
			title: qsTr("MCMC parameters")
			IntegerField
			{
				name: "noSamples"
				label: qsTr("No. samples")
				defaultValue: 2e3
				min: 10
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noBurnin"
				label: qsTr("No. burnin samples")
				defaultValue: 500
				min: 1
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noThinning"
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noChains"
				label: qsTr("No. chains")
				defaultValue: 3
				min: 1
				max: 50
				fieldWidth: 100
			}
		}

		RadioButtonGroup
		{
			name: "showResultsFor"
			title: qsTr("Show results for")
			RadioButton { value: "monitorAllParameters";		label: qsTr("all monitored parameters"); checked: true; id: monitorAllParameters	}
			RadioButton { value: "monitorSelectedParameters";	label: qsTr("selected parameters")													}
		}

		SetSeed{}

		CheckBox {	name: "showDeviance";	label: qsTr("Show Deviance");	checked: false	}
	}
}
