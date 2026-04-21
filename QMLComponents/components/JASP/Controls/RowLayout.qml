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
import QtQuick.Layouts

/*!
    \qmltype RowLayout
    \inqmlmodule JASP.Controls 1.0
    \brief A horizontal layout with JASP-themed spacing.

    Wraps Qt's RowLayout with default spacing from jaspTheme.rowGridSpacing
    and top-left alignment.

    \note RowLayout does not bind to R options. It is a layout-only control.

    \section1 Example

    \qml
    RowLayout {
        CheckBox { name: "mean"; label: qsTr("Mean") }
        CheckBox { name: "sd";   label: qsTr("Std. deviation") }
    }
    \endqml
*/
RowLayout
{
	spacing:				jaspTheme.rowGridSpacing
	Layout.alignment:		Qt.AlignTop | Qt.AlignLeft
}
