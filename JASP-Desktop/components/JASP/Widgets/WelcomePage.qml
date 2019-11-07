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
import QtQuick.Controls 2.4

import JASP.Widgets		1.0

FocusScope
{
	id:		welcomeRoot

	property real scaler: Math.max(0.9, Math.min(1.5, 0.8 * Math.min(welcomeRoot.width / info.baseWidth, welcomeRoot.height / info.baseHeight)))


	FontLoader { id: latoLightFontFamily;	source: "qrc:/core/font/Lato-Light.ttf" }
	FontLoader { id: latoRegularFontFamily;	source: "qrc:/core/font/Lato-Regular.ttf" }


	Rectangle
	{
		id:					centerPiece
		color:				jaspTheme.white
		height:				400 * welcomeRoot.scaler
		anchors
		{
			verticalCenter:	parent.verticalCenter
			left:			parent.left
			right:			parent.right
		}

		Item
		{
			id:					info
			z:					1
			anchors.centerIn:	parent
			height:				baseHeight * welcomeRoot.scaler
			width:				baseWidth  * welcomeRoot.scaler

			property int baseWidth:		700
			property int baseHeight:	500

			Image
			{
				id:				jaspLogo
				source:			jaspTheme.iconPath + "jasp-logo.svg"
				width:			(190 / 40) * height
				height:			info.height / 14
				mipmap:			true
				sourceSize
				{
					width:		jaspLogo.width  * 2
					height:		jaspLogo.height * 2
				}

				anchors
				{
					top:		parent.top
					left:		parent.left
				}
			}

			Text
			{
				id:				welcomeToJASP
				text:			qsTr("Welcome to JASP")
				color:			jaspTheme.white
				font.family:	latoRegularFontFamily.name
				font.pixelSize: 30 * welcomeRoot.scaler
				font.weight:	Font.Bold
				renderType:		Text.QtRendering

				anchors
				{
					top:				jaspLogo.bottom
					horizontalCenter:	parent.horizontalCenter
				}
			}

			Text
			{
				id:				version
				text:			mainWindow.versionString()
				color:			jaspTheme.white
				font.family:	latoLightFontFamily.name
				font.pixelSize: 14 * welcomeRoot.scaler
				font.weight:	Font.Normal
				renderType:		Text.QtRendering

				anchors
				{
					top:		parent.top
					right:		parent.right
				}
			}

			Rectangle
			{
				id:			jaspRuler
				color:		jaspTheme.white
				opacity:	0.5
				height:		4 * welcomeRoot.scaler
				anchors
				{
					top:		welcomeToJASP.bottom
					left:		parent.left
					right:		parent.right
					topMargin:	jaspTheme.generalAnchorMargin
				}
			}

			Text
			{
				id:				freshAndFunky
				text:			qsTr("A Fresh Way to Do Statistics: Free, Friendly, and Flexible")
				color:			jaspTheme.white
				font.family:	latoLightFontFamily.name
				font.pixelSize: 16 * welcomeRoot.scaler
				font.weight:	Font.Normal
				renderType:		Text.QtRendering

				anchors
				{
					top:				jaspRuler.bottom
					topMargin:			jaspRuler.anchors.topMargin
					horizontalCenter:	parent.horizontalCenter
				}
			}

			ListModel
			{
				id: bitingTheBullets

				ListElement { keyword: qsTr("Free:");		explanation: qsTr("JASP is an open-source project with structural support from the University of Amsterdam.");		}
				ListElement { keyword: qsTr("Friendly:");	explanation: qsTr("JASP has an intuitive interface that was designed with the user in mind.");						}
				ListElement { keyword: qsTr("Flexible:");	explanation: qsTr("JASP offers standard analysis procedures in both their classical and Bayesian manifestations.");	}
			}

			Component
			{
				id: bulletPointComp

				Item
				{
					width:					parent.width
					height:					Math.max(blueKeyword.height, explanationElement.height)

					Image
					{
						id:					orangeDot
						source:				jaspTheme.iconPath + "ul-orange-dot.png"
						width:				height
						height:				8 * welcomeRoot.scaler
						mipmap:				true

						anchors
						{
							verticalCenter:	blueKeyword.verticalCenter
							left:			parent.left
							margins:		1 //welcomeRoot.scaler
						}
					}

					TextArea
					{
						id:					blueKeyword
						text:				keyword
						font.family:		latoRegularFontFamily.name
						font.pixelSize:		explanationElement.font.pixelSize
						font.weight:		Font.ExtraBold
						verticalAlignment:	Text.AlignVCenter
						renderType:			Text.QtRendering
						color:				"#23a1df"
						width:				80 * welcomeRoot.scaler
						readOnly:			true
						selectByKeyboard:	false
						selectByMouse:		false
						anchors
						{
							top:			parent.top
							left:			orangeDot.right
							margins:		orangeDot.anchors.margins
						}
					}

					TextArea
					{
						id:					explanationElement
						text:				explanation
						font.family:		latoLightFontFamily.name
						font.pixelSize:		freshAndFunky.font.pixelSize
						font.weight:		Font.Light
						verticalAlignment:	Text.AlignVCenter
						color:				jaspTheme.black
						wrapMode:			TextEdit.Wrap
						readOnly:			true
						renderType:			Text.QtRendering
						selectByKeyboard:	false
						selectByMouse:		false
						anchors
						{
							top:			blueKeyword.top
							left:			blueKeyword.right
							right:			parent.right
						}
					}
				}
			}

			Column
			{
				id:							bulletPoints
				width:						parent.widthOverflowers
				height:						childrenRect.height
				spacing:					2 //* preferencesModel.uiScale
				anchors.horizontalCenter:	parent.horizontalCenter
				y:							(parent.height / 2) - (height / 2) - (20 * welcomeRoot.scaler)

				Repeater
				{
					model:				bitingTheBullets
					delegate:			bulletPointComp

				}
			}

			Text
			{
				id:						openADataFile
				text:					qsTr("So open a data file and take JASP for a spin!")
				color:					jaspTheme.black
				font.underline:			openDataFileMouse.containsMouse
				font.family:			latoRegularFontFamily.name
				font.pixelSize:			freshAndFunky.font.pixelSize + (2 * welcomeRoot.scaler)
				renderType:				Text.QtRendering
				anchors
				{
					horizontalCenter:	parent.horizontalCenter
					top:				bulletPoints.bottom
					topMargin:			25 * welcomeRoot.scaler
				}

				MouseArea
				{
					id:				openDataFileMouse
					anchors.fill:	parent
					hoverEnabled:	true
					onClicked:		fileMenuModel.showFileOpenMenu()
					cursorShape:	Qt.PointingHandCursor
				}

			}

			Rectangle
			{
				id:					downloadNewJASPButton
				color:				jaspTheme.blue
				radius:				height / 2
				height:				downloadNewJASP.height * 1.5
				width:				downloadNewJASP.width  * 1.2
				visible:			mainWindow.downloadNewJASPUrl !== ""

				anchors
				{
					top:					openADataFile.bottom
					topMargin:				(openADataFile.height * 2.5) - (height / 2)
					horizontalCenter:		openADataFile.horizontalCenter
				}

				Text
				{
					id:						downloadNewJASP
					anchors.centerIn:		parent
					text:					qsTr("Click to get latest version")
					font.family:			latoRegularFontFamily.name
					font.pixelSize:			openADataFile.font.pixelSize + (downloadMouseArea.containsMouse ? 4 * welcomeRoot.scaler : 0)
					font.weight:			Font.Bold
					color:					jaspTheme.white
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
				}

				MouseArea
				{
					id:						downloadMouseArea
					hoverEnabled:			true
					onClicked:				Qt.openUrlExternally(mainWindow.downloadNewJASPUrl);
					anchors.fill:			parent
					cursorShape:			Qt.PointingHandCursor
					focus:					true
				}
			}

			property int widthOverflowers:	width * 0.9

			TextArea
			{
				id:						keepInMindBeta
				text:					qsTr("Please keep in mind that this is a preview release and a number of features are still missing.\n\nIf JASP doesn’t do all you want today, then check back tomorrow: JASP is being developed at break-neck speed!")
				font.family:			latoLightFontFamily.name
				font.pixelSize:			12 * welcomeRoot.scaler
				font.weight:			Font.Normal
				color:					jaspTheme.white
				width:					parent.widthOverflowers
				wrapMode:				TextEdit.Wrap
				renderType:				Text.QtRendering
				readOnly:				true
				selectByKeyboard:		false
				selectByMouse:			false
				horizontalAlignment:	Text.AlignHCenter

				anchors
				{
					bottom:				parent.bottom
					bottomMargin:		-20
					horizontalCenter:	parent.horizontalCenter
				}
			}
		}


		Image
		{
			id:						blueWave
			fillMode:				Image.TileHorizontally
			horizontalAlignment:	Image.AlignHCenter
			height:					100  * welcomeRoot.scaler
			sourceSize.width:		1400 * welcomeRoot.scaler
			sourceSize.height:		height
			source:					jaspTheme.iconPath + "jasp-wave-down-blue-120.svg"
			anchors
			{
				top:				parent.top
				left:				parent.left
				right:				parent.right

			}
		}

		Image
		{
			id:						greenWave
			fillMode:				blueWave.fillMode
			horizontalAlignment:	Image.AlignHCenter
			height:					blueWave.height
			sourceSize.width:		blueWave.sourceSize.width
			sourceSize.height:		blueWave.sourceSize.height
			source:					jaspTheme.iconPath + "jasp-wave-up-green-120.svg"
			anchors
			{
				left:				parent.left
				right:				parent.right
				bottom:				parent.bottom

			}
		}
	}

	Rectangle
	{
		id:			blueBackgroundTop
		z:			-1
		color:		"#14a1e3"
		anchors
		{
			top:	parent.top
			left:	parent.left
			right:	parent.right
			bottom:	parent.verticalCenter
		}
	}

	Rectangle
	{
		id:			greenBackgroundTop
		z:			-1
		color:		"#8cc63e"
		anchors
		{
			top:	parent.verticalCenter
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}
	}
}
