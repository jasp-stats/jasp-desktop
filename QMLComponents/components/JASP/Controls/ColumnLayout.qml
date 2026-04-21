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
    \qmltype ColumnLayout
    \inqmlmodule JASP.Controls 1.0
    \brief A vertical layout container with JASP-themed spacing.

    Wraps Qt's ColumnLayout with default spacing and alignment suited for JASP analysis forms.
    Child items are arranged vertically from top to bottom.

    \note ColumnLayout does not bind to R options. It is a layout-only control.

    \section1 Properties

    \list
    \li \b spacing (real) - Vertical spacing between child items. Default: 5.
    \endlist

    \section1 Example

    \qml
    ColumnLayout {
        CheckBox { name: "option1"; label: qsTr("Option 1") }
        CheckBox { name: "option2"; label: qsTr("Option 2") }
    }
    \endqml
*/
ColumnLayout
{
	spacing:				jaspTheme.rowGridSpacing
	Layout.alignment:		Qt.AlignTop | Qt.AlignLeft
}
