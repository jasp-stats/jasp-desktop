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


Section
{
	expanded:	true
	title:		qsTr("Data")
	columns:	1

	property alias dataType: dataType

	RadioButtonGroup
	{
		columns:	3
		name:		"dataType"
		id:			dataType
		title:		qsTr("Input type")

		RadioButton
		{
			value:		"dataVariable"
			label:		qsTr("Select variable")
			id:			dataTypeC
			checked: 	mainWindow.dataAvailable
			enabled:	mainWindow.dataAvailable
		}

		RadioButton
		{
			value:		"dataCounts"
			label:		qsTr("Specify data summary")
			id:			dataTypeA
			checked:	!mainWindow.dataAvailable
		}

		RadioButton
		{
			value:		"dataSequence"
			label:		qsTr("Enter sequence")
			id:			dataTypeB
		}

	}

	Group
	{
		title: qsTr("Data summary")
		visible: dataTypeA.checked

		DoubleField
		{
			name:			"mean"
			label:			qsTr("Mean")
			defaultValue: 	0
		}

		DoubleField
		{
			name:			"SD_summary"
			label:			qsTr("SD")
			defaultValue:	0
			min:			0
			inclusive:		JASP.None
		}

		DoubleField
		{
			name:			"N"
			label:			qsTr("Observations")
			defaultValue:	0
			min:			0
			inclusive:		JASP.None
		}
	}

		
	TextArea
	{
		title:		qsTr("Comma-separated sequence of observations")
		visible:	dataTypeB.checked
		height:		100
		name:		"data_sequence"
		textType:	"source"
		separators:	[",",";","\n"]
	}

	DoubleField
	{
		name:			"SD_sequence"
		visible:		dataTypeB.checked
		label:			qsTr("SD")
		defaultValue:	0
		min:			0
		inclusive:		JASP.None
	}

	Group
	{
		visible: dataTypeC.checked

		VariablesForm
		{
			preferredHeight:	150

			AvailableVariablesList
			{
				name:	"allVariables"
				title:	qsTr("Available")
			}

			AssignedVariablesList
			{
				name:				"selectedVariable"
				title:				qsTr("Selected")
				singleVariable:		true
				allowedColumns:		["scale"]
			}

			DoubleField
			{
				name:			"SD_variable"
				label:			qsTr("SD")
				defaultValue:	0
				min:			0
				inclusive:		JASP.None
			}
		}

	}

	CheckBox
	{
		visible:	dataTypeB.checked || dataTypeC.checked
		name:		"dataSummary"
		label:		qsTr("Data summary")
		checked:	true
	}

}
