//
// Copyright (C) 2013-2025 University of Amsterdam
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


import JASP.Controls

/*!
    \qmltype SetSeed
    \inqmlmodule JASP.Controls 1.0
    \brief A preset "Repeatability" group with a seed checkbox and integer field.

    Provides a standardized Group titled "Repeatability" containing a
    "Set seed" CheckBox with an IntegerField for the seed value.
    Drop this into any analysis that uses random sampling.

    \section1 R Binding

    \list
    \li \b{R Options:}
        \list
        \li \c setSeed (bool) — Whether seeding is enabled
        \li \c seed (integer) — The seed value
        \endlist
    \endlist

    \section1 Example

    \qml
    SetSeed {}
    \endqml
*/
Group
{
	title: qsTr("Repeatability")

	CheckBox {
		name: "setSeed"
		text: qsTr("Set seed:")
		childrenOnSameRow: true

		IntegerField {
			name: "seed"
			defaultValue: 1
			min: -999999
			max: 999999
			fieldWidth: 60
		}
	}
}
