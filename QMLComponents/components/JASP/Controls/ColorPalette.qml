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


import QtQuick
import JASP.Controls

/*!
    \qmltype ColorPalette
    \inqmlmodule JASP.Controls 1.0
    \brief A dropdown preset for selecting a color palette for plots.

    Extends DropDown with a predefined list of color palettes commonly used in JASP plots.
    Includes colorblind-friendly, Viridis, ggplot2, and other standard palettes.
    Defaults to "colorblind" and binds to the R option "colorPalette".

    \section1 R Binding

    \list
    \li \b{R Type:} \c character
    \li \b{Default:} "colorblind"
    \endlist

    \section1 Inherited Properties from DropDown

    \list
    \li \b name (string) - R option name this control binds to. Default: "colorPalette".
    \li \b label (string) - Label displayed before the dropdown. Default: "Color palette".
    \li \b values (array) - List of palette options. Pre-populated with standard palettes.
    \li \b currentValue (var) - The value of the currently selected palette.
    \endlist

    \section1 Other Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    ColorPalette { }
    \endqml
*/
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
		{ label: qsTr("JASP"),				value: "jaspPalette"			},
		{ label: qsTr("Viridis"),			value: "viridis"		},
		{ label: qsTr("ggplot2"),			value: "ggplot2"		},
		{ label: qsTr("Gray"),				value: "gray"			},
		{ label: qsTr("Blue"),				value: "blue"			},
		{ label: qsTr("Sports teams: NBA"),	value: "sportsTeamsNBA"	},
		{ label: qsTr("Grand Budapest"),	value: "grandBudapest"	}
	]
}
