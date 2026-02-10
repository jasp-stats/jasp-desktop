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
import QtQuick.Controls as QtC

/*!
    \qmltype Label
    \inqmlmodule JASP.Controls 1.0
    \brief A JASP-themed text label.

    Wraps Qt's Label with the JASP default font and color scheme. Text color
    automatically adjusts when the control is disabled.

    \note Label does not bind to R options. It is a display-only control.

    \section1 Properties

    \list
    \li \b text (string) - Text to display. Default: "".
    \endlist

    \section1 Example

    \qml
    Label { text: qsTr("Effect size:") }
    \endqml
*/
QtC.Label
{
	font:	jaspTheme.font
	color:	enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
}
