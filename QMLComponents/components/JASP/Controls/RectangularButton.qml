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

import QtQuick 2.9
import QtQuick.Controls 2.4


Rectangle
{
	id:				filterButtonRoot

	property alias  textFormat:			buttonText.textFormat
	property string	text:				""
	property string	toolTip:			""
	property string textColor:			"default"
	property bool	selected:			activeFocus
	property string	iconSource:			""
	property real	buttonPadding:		6 * preferencesModel.uiScale
	property real	buttonWidthPadding:	buttonPadding
	property alias	hovered:			buttonMouseArea.containsMouse
	property bool	showIconAndText:	false
	property bool	centerText:			true
	property bool	iconLeft:			true
	property bool	isLink:				false
	property bool	centerTextParent:	false

	property real	_scaledDim:			Math.max(jaspTheme.defaultRectangularButtonHeight, buttonText.height + 2 * buttonPadding)
	property alias	_pressed:			buttonMouseArea.pressed
	property alias	font:				buttonText.font
	property alias	icon:				buttonIcon
	property real	centerParentX:		(parent.width / 2) - x

	//on_ScaledDimChanged: console.log("Button " + text + ": " + _scaledDim + ", text height: " + buttonText.height + ", content height: " + buttonText.contentHeight + ", padding: " + buttonPadding)

	focus:								true
	implicitWidth:						showIconAndText ?
											buttonText.implicitWidth + buttonWidthPadding + _scaledDim + buttonWidthPadding :
											buttonIcon.visible ? _scaledDim : buttonText.implicitWidth + ( 2 * buttonWidthPadding)
	implicitHeight:						_scaledDim
	width:								implicitWidth
	height:								implicitHeight
	color:								!enabled ? jaspTheme.buttonColorDisabled
												 : _pressed ? jaspTheme.buttonColorPressed
															: (filterButtonRoot.hovered || filterButtonRoot.activeFocus)	? jaspTheme.buttonColorHovered
																															: jaspTheme.buttonColor
	border.color:						(filterButtonRoot.hovered || selected) ? jaspTheme.buttonBorderColorHovered
																			   : jaspTheme.buttonBorderColor
	border.width:						1


	ToolTip.text:						toolTip
	ToolTip.timeout:					jaspTheme.toolTipTimeout
	ToolTip.delay:						jaspTheme.toolTipDelay
	ToolTip.visible:					toolTip !== "" && buttonMouseArea.containsMouse

	Keys.onSpacePressed:				clicked();
	Keys.onEnterPressed:				clicked();
	Keys.onReturnPressed:				(event)=>	clicked();

	signal clicked()



	MouseArea
	{
		id:							buttonMouseArea
		anchors.fill:				parent
		acceptedButtons:			Qt.LeftButton
		hoverEnabled:				true
		cursorShape:				Qt.PointingHandCursor
		onClicked:					filterButtonRoot.clicked();
		visible:					filterButtonRoot.enabled
		//propagateComposedEvents:	true
	}

	Image
	{
		id:					buttonIcon
		x:					!filterButtonRoot.showIconAndText 
							?	(parent.width / 2) - (width / 2) 
							:	filterButtonRoot.iconLeft 
							?	filterButtonRoot.buttonWidthPadding 
							:	parent.width - (width + filterButtonRoot.buttonWidthPadding)
		y:					(parent.height / 2) - (height / 2)

		width:				Math.min(filterButtonRoot.width - (2 * buttonWidthPadding), height)
		height:				filterButtonRoot.height - (2 * buttonPadding)

		visible:			filterButtonRoot.iconSource != "" || filterButtonRoot.showIconAndText
		source:				filterButtonRoot.iconSource
		sourceSize.width:	width  * 2
		sourceSize.height:	height * 2
		mipmap:				true
		smooth:				true
	}

	Text
	{
		id: buttonText
		x:	!filterButtonRoot.centerText 
			?	filterButtonRoot.buttonPadding
			:	filterButtonRoot.centerTextParent
				? (centerParentX - (contentWidth / 2))
				: ((parent.width / 2) - (contentWidth / 2) )

		y:	(parent.height / 2) - (height / 2)

		text:		filterButtonRoot.text
		wrapMode:	Text.NoWrap
		visible:	filterButtonRoot.iconSource == "" || filterButtonRoot.showIconAndText
		color:		isLink
						? (enabled ? jaspTheme.blueDarker : jaspTheme.textDisabled)
						: (textColor == "default"
							? (filterButtonRoot.enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled)
							: textColor)


		font:	isLink ? jaspTheme.fontLink : jaspTheme.font
		width:	filterButtonRoot.width - (!filterButtonRoot.centerText ?	filterButtonRoot.buttonPadding : 0)
		elide:	Text.ElideMiddle
	}
}

