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
import QtQuick			2.11
import JASP				1.0

Item
{
	id				: crossRectangle
	width			: 12 * jaspTheme.uiScale
	height			: 12 * jaspTheme.uiScale
	anchors.top		: parent.top
	anchors.right	: parent.right

	property int crossThickness		: (crossArea.containsMouse ? 3 : 2) * jaspTheme.uiScale
	property int crossLengthOffset	: 2 * jaspTheme.uiScale

	property bool warning: false

	signal crossClicked();

	Rectangle
	{
		anchors.centerIn	: parent
		height				: crossRectangle.crossThickness
		width				: parent.width - crossRectangle.crossLengthOffset
		rotation			: 45
		color				: warning ? jaspTheme.controlWarningTextColor : jaspTheme.controlErrorTextColor
	}

	Rectangle
	{
		anchors.centerIn	: parent
		height				: crossRectangle.crossThickness
		width				: parent.width - crossRectangle.crossLengthOffset
		rotation			: -45
		color				: warning ? jaspTheme.controlWarningTextColor : jaspTheme.controlErrorTextColor
	}

	MouseArea
	{
		id				: crossArea
		anchors.fill	: parent
		onClicked		: crossRectangle.crossClicked()
		hoverEnabled	: true
		cursorShape		: Qt.PointingHandCursor
	}
}

