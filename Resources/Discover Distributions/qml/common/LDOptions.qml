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

Group
{
	id: options
	property bool negativeValues			: true
	property double	min						: negativeValues ? -Infinity : 0
	property double	max						: Infinity
	property double rangeMinX				: min === -Infinity ? -3 : min
	property double rangeMaxX				: max === Infinity ? 3 : max
	property double intervalMinmaxMin		: 0
	property double intervalMinmaxMax		: 1
	property double intervalLowerMax		: 0
	property double intervalUpperMin		: 0

	title: qsTr("Options")
	Row
	{
		spacing: jaspTheme.columnGroupSpacing
		DoubleField
		{
			name: "min_x"; label: qsTr("Range of x from"); id: min_x;
			defaultValue: options.rangeMinX; min: options.min; max: parseFloat(max_x.value)
		}
		DoubleField
		{
			name: "max_x"; label: qsTr("to"); id: max_x;
			defaultValue: options.rangeMaxX; min: parseFloat(min_x.value); max: options.max
		}
	}

	Group
	{
		title: qsTr("Highlight")
		Group
		{
			columns: 2
			CheckBox{ name: "highlightDensity"		; label: qsTr("Density")	; id: highlightDensity }
			CheckBox{ name: "highlightProbability"	; label: qsTr("Probability"); id: highlightProbability }
		}

		RadioButtonGroup
		{
			name: "highlightType"
			title: qsTr("Interval")
			enabled: highlightDensity.checked || highlightProbability.checked
			GridLayout
			{
				columns: 3
				rowSpacing: jaspTheme.rowGroupSpacing
				columnSpacing: 0

				RadioButton { value: "minmax"; checked: true; id: minmax }
				DoubleField { name: "min"; label: qsTr("from")	; min: options.min; max: parseFloat(minmaxMax.value); defaultValue: options.intervalMinmaxMin; id: minmaxMin; enabled: minmax.checked }
				DoubleField { name: "max"; label: qsTr("to")	; min: parseFloat(minmaxMin.value); max: options.max; defaultValue: options.intervalMinmaxMax; id: minmaxMax; enabled: minmax.checked; Layout.leftMargin: jaspTheme.columnGroupSpacing }

				RadioButton { value: "lower"; id: lower }
				Label		{ text: qsTr("from %1").arg(options.min === -Infinity ? " -∞" : (" " + options.min)); enabled: lower.checked }
				DoubleField { name: "lower_max"; label: qsTr("to"); min: options.min; max: options.max; defaultValue: options.intervalLowerMax; enabled: lower.checked; Layout.leftMargin: jaspTheme.columnGroupSpacing }

				RadioButton { value: "upper"; id: upper }
				DoubleField { name: "upper_min"; label: qsTr("from"); defaultValue: options.intervalUpperMin; min: options.min; max: options.max; enabled: upper.checked }
				Label		{ text: qsTr("to %1").arg(options.max === Infinity ? " ∞" : (" "  + options.max)); Layout.leftMargin: jaspTheme.columnGroupSpacing; enabled: upper.checked }
			}
		}
	}
}
