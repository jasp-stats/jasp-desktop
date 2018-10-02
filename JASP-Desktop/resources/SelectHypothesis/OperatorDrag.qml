//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

import QtQuick 2.0


DragGeneric {
	shownChild                               : showMe
	property string       __debugName        : "OperatorDrag"

	readonly property var everythingOperators: ["==", "<"]
	property string       operator           : "+"
	property          var opImages           : { '==': 'qrc:/icons/equal.png', '<': 'qrc:/icons/lessThan.png'}
	property bool         acceptsDrops       : true

	leftDropSpot                             : showMe.leftDropSpot
	property alias leftDrop                  : showMe.leftDrop
	property alias rightDrop                 : showMe.rightDrop

	Operator
	{
		id                     : showMe
		operator               : parent.operator
		operatorImageSource    : parent.opImages[operator] !== null && parent.opImages[operator] !== undefined ? parent.opImages[operator] : ""

		dropKeysLeft           : ["boolean", "string", "number"]
		dropKeysRight          : ["boolean", "string", "number"]
		dropKeysMirrorEachother: true

		x                      : parent.dragX
		y                      : parent.dragY
		isNested               : parent.nested

		acceptsDrops           : parent.acceptsDrops
	}
}
