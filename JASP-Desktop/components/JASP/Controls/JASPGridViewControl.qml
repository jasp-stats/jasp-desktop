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
	id						: jaspGridViewControl
	controlType				: JASPControlBase.VariablesListView
	background				: itemRectangle
	implicitWidth 			: parent.width
	implicitHeight			: jaspTheme.defaultVariablesFormHeight
	useControlMouseArea		: false
	shouldStealHover		: false
	innerControl			: itemGridView

	property var	model
	property var	values
	property string title
	property alias	label				: jaspGridViewControl.title
	property alias	count				: itemGridView.count
	property string	optionKey			: "value"
	property int	columns				: 1
	property var	source
	property var	sourceModel
	property alias	syncModels			: jaspGridViewControl.source

	property alias	listGridView		: itemGridView // Backward compatibility
	property alias	itemGridView		: itemGridView
	property alias	cellHeight			: itemGridView.cellHeight
	property alias	cellWidth			: itemGridView.cellWidth
	property alias	listRectangle		: itemRectangle // Backward compatibility
	property alias	itemRectangle		: itemRectangle
	property alias	scrollBar			: scrollBar
	property alias	listTitle			: itemTitle		// Backward compatibility
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
		anchors.top		: jaspGridViewControl.top;
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
		height			: jaspGridViewControl.height - itemTitle.height
		width			: parent.width
		color			: debug ? jaspTheme.debugBackgroundColor : jaspTheme.controlBackgroundColor
		border.width	: 1
		border.color	: jaspTheme.borderColor
		radius			: jaspTheme.borderRadius

		JASPScrollBar
		{
			id				: scrollBar
			flickable		: itemGridView
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

		GridView
		{
			id						: itemGridView
			cellHeight				: 20  * preferencesModel.uiScale
			cellWidth				: width / jaspGridViewControl.columns
			clip					: true
			focus					: true
			anchors.fill			: parent
			anchors.margins			: 4 * preferencesModel.uiScale
			anchors.rightMargin		: scrollBar.width + anchors.margins
			model					: jaspGridViewControl.model
			delegate				: jaspGridViewControl.itemComponent
			boundsBehavior			: Flickable.StopAtBounds
		}
	}
}
