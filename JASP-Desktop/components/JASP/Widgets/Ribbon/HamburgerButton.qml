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

import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Theme 1.0

Rectangle {
	id							: ribbonButton
	width						: implicitWidth
	height						: implicitHeight
	implicitHeight				: Theme.ribbonButtonHeight
	implicitWidth				: implicitHeight
	radius						: 5
	color						: mice.pressed ? Theme.grayLighter : Theme.uiBackground //mice.pressed ? Theme.grayLighter : mice.containsMouse ? Theme.white : Theme.uiBackground

	signal clicked

	Item
	{
		id:					hamburgerArrow
		anchors.centerIn:	parent
		width:				hamburgerArrow.barWidth//parent.width	- (2 * Theme.ribbonButtonPadding)
		height:				parent.height	- (2 * Theme.ribbonButtonPadding)
		scale:				mice.containsMouse && !mice.pressed ? Theme.ribbonScaleHovered : 1

		property real	barThickness:	8;//(Theme.ribbonButtonHeight (2 * Theme.ribbonButtonPadding)) / 7
		property real	barRadius:		barThickness
		property real	barWidth:		parent.width / 2
		property color	barColor:		Theme.grayDarker
		property real	offsetY:		!fileMenuModel.visible ? 0 : (height / 8) //+ (barThickness / 2)
		property real	offsetX:		!fileMenuModel.visible ? 0 : width / 4

		Item
		{
			id:		topBar
			x:		-hamburgerArrow.offsetX
			y:		Theme.ribbonButtonPadding + hamburgerArrow.offsetY //(parent.height / 3) - (height * 0.5)
			height:	hamburgerArrow.barThickness
			width:	parent.width

			Rectangle
			{
				anchors.centerIn:	parent
				height:				parent.height
				width:				!fileMenuModel.visible ? parent.width : parent.width / 1.25
				rotation:			!fileMenuModel.visible ? 0 : -45
				radius:				hamburgerArrow.barRadius
				color:				hamburgerArrow.barColor
			}
		}

		Rectangle
		{
			id:		middleBar
			height:	hamburgerArrow.barThickness
			width:	parent.width
			radius:	hamburgerArrow.barRadius
			color:	hamburgerArrow.barColor

			anchors.centerIn:	parent
		}

		Item
		{
			id:		bottomBar
			x:		-hamburgerArrow.offsetX
			y:		parent.height - Theme.ribbonButtonPadding - height - hamburgerArrow.offsetY //(parent.height / 3)) - (height * 0.5)
			height:	hamburgerArrow.barThickness
			width:	parent.width

			Rectangle
			{
				anchors.centerIn:	parent
				height:				parent.height
				width:				!fileMenuModel.visible ? parent.width : parent.width / 1.25
				rotation:			!fileMenuModel.visible ? 0 : 45
				radius:				hamburgerArrow.barRadius
				color:				hamburgerArrow.barColor
			}
		}

		MouseArea
		{
			id				: mice
			anchors.fill	: parent
			hoverEnabled	: true
			acceptedButtons	: Qt.LeftButton
			onClicked		: fileMenuModel.visible = !fileMenuModel.visible
		}
	}
}
