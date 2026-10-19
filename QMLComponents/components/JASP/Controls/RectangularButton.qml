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
import QtQuick.Controls


/*!
    \qmltype RectangularButton
    \inqmlmodule JASP.Controls 1.0
    \brief A styled rectangular button with optional icon and text.

    A Rectangle-based button supporting text, icon, or both. Provides hover,
    pressed, and disabled states with JASP theming. Used as the base for
    RoundedButton and MenuButton.

    \note This is primarily an internal UI component. Module developers typically
    use Button instead.

    \section1 Properties

    \list
    \li \b text (string) - Button label text. Default: "".
    \li \b toolTip (string) - Tooltip shown on hover. Default: "".
    \li \b iconSource (string) - Path to the button icon. Default: "".
    \li \b showIconAndText (bool) - Show both icon and text simultaneously. Default: false.
    \li \b centerText (bool) - Center the text within the button. Default: true.
    \li \b iconLeft (bool) - Place icon on the left side. Default: true.
    \li \b isLink (bool) - Style as a hyperlink. Default: false.
    \endlist

    \section1 Signals

    \list
    \li \b clicked() - Emitted when the button is clicked.
    \endlist

    \section1 Example

    \qml
    RectangularButton {
        text: qsTr("Apply")
        iconSource: jaspTheme.iconPath + "confirm.png"
    }
    \endqml
*/
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
	property color	defaultColor:		!enabled ? jaspTheme.buttonColorDisabled
												 : _pressed ? jaspTheme.buttonColorPressed
															: (filterButtonRoot.hovered || filterButtonRoot.activeFocus)	? jaspTheme.buttonColorHovered
																															: (typeof jaspForm === 'undefined') ? jaspTheme.uiBackground
																																								: jaspTheme.buttonColor
	property color defaultBorderColor:	enabled && (filterButtonRoot.hovered || selected)	? jaspTheme.buttonBorderColorHovered
																							: jaspTheme.buttonBorderColor

	//on_ScaledDimChanged: console.log("Button " + text + ": " + _scaledDim + ", text height: " + buttonText.height + ", content height: " + buttonText.contentHeight + ", padding: " + buttonPadding)

	implicitWidth:						showIconAndText ?
											buttonText.implicitWidth + buttonWidthPadding + _scaledDim + buttonWidthPadding :
											buttonIcon.visible ? _scaledDim : buttonText.implicitWidth + ( 2 * buttonWidthPadding)
	implicitHeight:						_scaledDim
	width:								implicitWidth
	height:								implicitHeight
	color:								defaultColor
	border.color:						defaultBorderColor
	border.width:						1

	ToolTip.text:						toolTip
	ToolTip.visible:					toolTip !== "" && buttonMouseArea.containsMouse
	ToolTip.toolTip.background:			Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }

	Keys.onSpacePressed:				clicked();
	Keys.onEnterPressed:				clicked();
	Keys.onReturnPressed:				(event)=>	clicked();

	signal clicked()
	signal doubleClicked()



	MouseArea
	{
		id:							buttonMouseArea
		anchors.fill:				parent
		acceptedButtons:			Qt.LeftButton
		hoverEnabled:				true
		cursorShape:				filterButtonRoot.enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
		onClicked:					{ parent.forceActiveFocus(); filterButtonRoot.clicked() }
		onDoubleClicked:			{ parent.forceActiveFocus(); filterButtonRoot.doubleClicked() }
		//visible:					filterButtonRoot.enabled
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

