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
import JASP.Controls
import JASP.Widgets
import QtQuick.Layouts

FocusScope
{
	id:		osfLogin
	
	property bool minimal: osfLogin.height < 450 * preferencesModel.uiScale
	

	Component.onCompleted:
	{
		usernameText.focus = true
	}
	
	ColumnLayout
	{
			
		id:						osfLogoColumn
		width:					250 * preferencesModel.uiScale
		anchors
		{
			top:				parent.top
			bottom:				parent.bottom
			horizontalCenter:	parent.horizontalCenter
		}
		
		Item
		{
			implicitHeight:		(osfLogin.minimal ? 40 : 160) * preferencesModel.uiScale 
			Layout.fillWidth:	true

			Image
			{
				id:					osfLogo
		
				anchors.fill:		parent
				
				source:				jaspTheme.iconPath + "osf-logo.png"
				smooth:				true
				fillMode:			Image.PreserveAspectFit
		
				sourceSize.width:	height * 2
				sourceSize.height:	height * 2
		
				Layout.alignment:	Qt.AlignHCenter
			}
		}
	
		Label
		{
			id: labelOSF
	
			Layout.fillWidth:	true
	
			verticalAlignment  : Text.AlignVCenter
			horizontalAlignment: Text.AlignHCenter
	
			text : qsTr("OSF")
			
			color: jaspTheme.black
			font : jaspTheme.fontLabel
		}
	
		Text
		{
			id:						labelExplain
			text :					qsTr("Sign in with your OSF account to continue")
			color:					jaspTheme.textEnabled
			font.pointSize:			(osfLogin.minimal ? 8 : 11) * preferencesModel.uiScale
			font.family:			jaspTheme.font.family
			verticalAlignment:		Text.AlignVCenter
			horizontalAlignment:	Text.AlignHCenter
			Layout.fillWidth:		true

		}
	
		Rectangle
		{
			id:					osfLoginBox
			Layout.fillWidth:	true
			implicitHeight:		(osfLogin.minimal ? 160 : 200) * preferencesModel.uiScale
			color:				jaspTheme.grayMuchLighter
			border.width:		1
			border.color:		jaspTheme.grayDarker
	
	
			ColumnLayout
			{
				id:					loginColumn
				anchors.fill:		parent
				anchors.margins:	jaspTheme.generalAnchorMargin
				spacing:			(height - (buttonHeight * 4)) / 4
				
				property real buttonHeight:	35 * preferencesModel.uiScale
				
				Item
				{
					implicitHeight:			loginColumn.buttonHeight
					Layout.fillWidth:		true
					
					Rectangle
					{
						id:					usernameInput
			
						clip:				true
						color:				jaspTheme.white
						border.width:		usernameText.activeFocus ? 3 : 1
						border.color:		usernameText.activeFocus ? jaspTheme.focusBorderColor : jaspTheme.grayDarker
						anchors.fill:		parent
			
						TextInput
						{
							id					: usernameText
							text				: fileMenuModel.osf.username
			
							anchors.fill		: parent
							anchors.leftMargin	: 10 * preferencesModel.uiScale
							selectByMouse		: true
							selectedTextColor	: jaspTheme.textDisabled
							selectionColor		: jaspTheme.itemSelectedColor
							color				: jaspTheme.textEnabled
			
			
							verticalAlignment	: Text.AlignVCenter
							font				: jaspTheme.fontRibbon
			
							onTextChanged		: fileMenuModel.osf.username = text
							onAccepted			: passwordText.focus = true
			
							KeyNavigation.tab	: passwordText
							focus				: true
						}
					}
			
					Text
					{
						text			: "<sup>*</sup>"
						textFormat		: Text.RichText
						font.pointSize	: 12 * preferencesModel.uiScale
						font.family		: jaspTheme.font.family
						color			: jaspTheme.textEnabled
			
						anchors
						{
							top:				usernameInput.top
							left:				usernameInput.right
							leftMargin:			2 * jaspTheme.uiScale
						}
					}
				}
		
				Rectangle
				{
					id:					passwordInput
		
					implicitHeight:		loginColumn.buttonHeight
					Layout.fillWidth:	true
					clip:				true
					color:				jaspTheme.white
		
					border.width:		passwordText.activeFocus ? 3 : 1
					border.color:		passwordText.activeFocus ? jaspTheme.focusBorderColor  : jaspTheme.grayDarker
		
					TextInput
					{
						id:						passwordText
		
						text:					fileMenuModel.osf.password
						anchors.fill:			parent
						anchors.leftMargin:		10 * preferencesModel.uiScale
						verticalAlignment:		Text.AlignVCenter
						echoMode:				TextInput.Password
						selectByMouse:			true
						selectedTextColor:		jaspTheme.textDisabled
						selectionColor:			jaspTheme.itemSelectedColor
						color:					jaspTheme.textEnabled
		
						font.pixelSize:			14 * preferencesModel.uiScale
		
		
						onTextChanged:	fileMenuModel.osf.password = text;
						onAccepted:		fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, fileMenuModel.osf.password)
		
						KeyNavigation.tab		: loginButton
		
					}
				}
		
				RoundedButton
				{
					id:					loginButton
		
					implicitHeight:		loginColumn.buttonHeight
					Layout.fillWidth:	true
					text:				qsTr("Sign in")
					color:				jaspTheme.jaspGreen
					border.width:		loginButton.activeFocus ? 3 : 1
					border.color:		loginButton.activeFocus ? jaspTheme.focusBorderColor : jaspTheme.grayDarker
		
					textColor:			jaspTheme.white

		
					onClicked:			fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, fileMenuModel.osf.password)
		
					KeyNavigation.tab: idRememberMe
				}
		
				CheckBox
				{
					id:						idRememberMe
					checked:				fileMenuModel.osf.rememberme
					label:					qsTr("Remember me")
					onCheckedChanged:		fileMenuModel.osf.rememberme = checked
					KeyNavigation.tab:		usernameText
					Layout.fillWidth:		true
				}
			}
		}
	
		Item
		{
			id:						linksBox
	
			Layout.fillWidth:		true
			implicitHeight:			Math.max(linkOSF.height, linkRegister.height) + noteText.height + jaspTheme.generalAnchorMargin
	
			Text 
			{
				id: linkOSF
	
				text			: qsTr("About the OSF")
				textFormat		: Text.StyledText
				font.pointSize	: 11 * preferencesModel.uiScale
				font.family		: jaspTheme.font.family
				font.underline	: true
				color			: jaspTheme.blueDarker
	
				anchors
				{
					bottomMargin:	jaspTheme.generalAnchorMargin
					bottom:			noteText.top
					right:			parent.horizontalCenter
					left:			parent.left
				}
	
				MouseArea
				{
					anchors.fill: parent
					hoverEnabled: true
					cursorShape : Qt.PointingHandCursor
					onClicked   : Qt.openUrlExternally("http://help.osf.io")
				}
			}
	
			Text
			{
				id: linkRegister
	
				text:					qsTr("Register")
				textFormat:				Text.StyledText
				font.pointSize:			11 * preferencesModel.uiScale
				font.family:			jaspTheme.font.family
				font.underline:			true
				color:					jaspTheme.blueDarker
				horizontalAlignment:	Text.AlignRight
	
				anchors
				{
					bottomMargin:	jaspTheme.generalAnchorMargin
					bottom:			noteText.top
					right:			parent.right
					left:			parent.horizontalCenter
				}

				MouseArea
				{
					anchors.fill: parent
					hoverEnabled: true
					cursorShape : Qt.PointingHandCursor
					onClicked   : Qt.openUrlExternally("https://osf.io")
				}
			}
	
			Text
			{
				id:					noteText
				text			: qsTr("<sup>*</sup>OSF account login only, ORCID or institutional login are not supported.")
				textFormat		: Text.RichText
				font.pointSize	: 8 * preferencesModel.uiScale
				font.family		: jaspTheme.font.family
				font.italic		: true
				color			: jaspTheme.textEnabled
				wrapMode		: Text.Wrap
	
				anchors
				{
					bottom:				parent.bottom
					left:				parent.left
					right:				parent.right
				}
			}
		}

	}
}
