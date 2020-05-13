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


import QtQuick		2.11
import JASP.Widgets 1.0
import JASP			1.0

JASPControl
{
	id						: jaspGridControl
	controlType				: JASPControlBase.VariablesListView
	background				: itemRectangle
	implicitWidth 			: parent.width
	implicitHeight			: itemTitle.height + itemGrid.height + 8 * preferencesModel.uiScale
	useControlMouseArea		: false
	shouldStealHover		: false
	innerControl			: itemGrid

	onPreferredHeightChanged:
	{
		if (preferredHeight != implicitHeight)
		{
			// preferredHeight has been specifically set
			implicitHeight = implicitHeight; // Break binding
			height = preferredHeight;
			itemRectangle.height = Qt.binding(function() { return preferredHeight - itemTitle.height; })
		}
	}

	property var	model
	property var	values
	property string title
	property alias	label				: jaspGridControl.title
	property alias	count				: itemRepeater.count
	property string	optionKey			: "value"
	property alias	columns				: itemGrid.columns
	property alias	rows				: itemGrid.rows
	property var	source
	property var	sourceModel
	property alias	syncModels			: jaspGridControl.source

	property alias	itemGrid			: itemGrid
	property alias	itemRectangle		: itemRectangle
	property alias	itemScrollbar		: itemScrollbar
	property alias	itemTitle			: itemTitle
	property alias	rowComponentsTitles	: itemTitles.model
	property alias	rowComponentsLabels	: itemTitles.model
	property int	rowComponentsSpacing : 1

	property var	itemComponent

	Text
	{
		id				: itemTitle
		anchors.top		: parent.top
		anchors.left	: parent.left
		text			: title
		height			: title ? jaspTheme.listTitle : 0
		font			: jaspTheme.font
		color			: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Row
	{
		width			: parent.width
		anchors.top		: jaspGridControl.top;
		spacing			: 5
		layoutDirection	: Qt.RightToLeft
		Repeater
		{
			id	: itemTitles;
			Label { text : modelData }
		}
	}

	Rectangle
	{
		id				: itemRectangle
		anchors.top		: itemTitle.bottom
		anchors.left	: parent.left
		height			: jaspGridControl.height - itemTitle.height
		width			: parent.width
		color			: debug ? jaspTheme.debugBackgroundColor : jaspTheme.analysisBackgroundColor
		border.width	: 1
		border.color	: jaspTheme.borderColor
		radius			: jaspTheme.borderRadius

		JASPScrollBar
		{
			id				: itemScrollbar
			flickable		: itemFlickable
			manualAnchor	: true
			vertical		: true
			z				: 1337

			anchors
			{
				top			: parent.top
				right		: parent.right
				bottom		: parent.bottom
				margins		: 2
			}
		}

		Flickable
		{
			id						: itemFlickable
			anchors.fill			: parent
			anchors.margins			: 4 * preferencesModel.uiScale
			anchors.rightMargin		: itemScrollbar.width + anchors.margins
			clip					: true
			boundsBehavior			: Flickable.StopAtBounds
			contentWidth			: itemGrid.width
			contentHeight			: itemGrid.height

			Grid
			{
				id						: itemGrid
				focus					: true
				columns					: 1

				Repeater
				{
					id						: itemRepeater
					model					: jaspGridControl.model
					delegate				: jaspGridControl.itemComponent
				}
			}
		}
	}
}
