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

Section
{
	title: enabled ? qsTr("Estimate Parameters") : qsTr("Estimate Parameters") + " - " + qsTr("[requires a loaded data set]")

	Group
	{
		CheckBox{ name: "methodMLE";      label: qsTr("Maximum likelihood"); visible: true  }
	}

	Group
	{
		title: qsTr("Output")
		CheckBox
		{
			name: "outputEstimates"; label: qsTr("Estimates"); checked: true
			CheckBox{ name: "outputSE"; label: qsTr("Std. error"); checked: false }
			CheckBox
			{
				name: "ciInterval"; label: qsTr("Confidence interval"); childrenOnSameRow: true
				PercentField{ name: "ciIntervalInterval"; label: ""; defaultValue: 95 }
			}
		}
	}
}
