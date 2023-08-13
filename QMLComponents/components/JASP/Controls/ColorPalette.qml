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
import JASP.Controls 1.0

DropDown
{
	name: "colorPalette"
	label: qsTr("Color palette")
	indexDefaultValue: 0
	values:
	[
		{ label: qsTr("Colorblind"),		value: "colorblind"		},
		{ label: qsTr("Colorblind #2"),		value: "colorblind2"	},
		{ label: qsTr("Colorblind #3"),		value: "colorblind3"	},
		{ label: qsTr("Viridis"),			value: "viridis"		},
		{ label: qsTr("ggplot2"),			value: "ggplot2"		},
		{ label: qsTr("Gray"),				value: "gray"			},
		{ label: qsTr("Blue"),				value: "blue"			},
		{ label: qsTr("Sports teams: NBA"),	value: "sportsTeamsNBA"	},
		{ label: qsTr("Grand Budapest"),	value: "grandBudapest"	}
	]
}
