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
import QtQuick.Layouts	1.3

import JASP.Widgets		1.0

FocusScope
{
	id:		welcomeRoot

	property real scaler: Math.max(0.9, Math.min(1.5, 0.9 * Math.min(welcomeRoot.width / info.baseWidth, welcomeRoot.height / info.baseHeight)))


	FontLoader { id: latoLightFontFamily;	source: "qrc:/html/font/Lato-Light.ttf" }
	FontLoader { id: latoRegularFontFamily;	source: "qrc:/html/font/Lato-Regular.ttf" }


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
				font.family:	jaspTheme.font
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
				font.family:	jaspTheme.font
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
				font.family:	jaspTheme.font
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

			Component
			{
				id:					orangeDot

				Image
				{
					Layout.topMargin: 6 * welcomeRoot.scaler
					Layout.alignment:	Qt.AlignLeft | Qt.AlignTop
					source:		jaspTheme.iconPath + "ul-orange-dot.png"
					height:		8 * welcomeRoot.scaler
					width:		8 * welcomeRoot.scaler
					mipmap:		true
				}
			}

			Component
			{
				id:					blueKeyword

				Text
				{
					Layout.alignment:	Qt.AlignLeft | Qt.AlignTop
					text:				modelData
					font.family:		jaspTheme.font
					font.pixelSize:		freshAndFunky.font.pixelSize
					//font.weight:		Font.Bold
					verticalAlignment:	Text.AlignVCenter
					renderType:			Text.QtRendering
					color:				"#23a1df"
				}
			}

			Component
			{
				id:						explanationElement

				Text
				{
					Layout.leftMargin:	10
					Layout.alignment:	Qt.AlignLeft | Qt.AlignTop
					text:				modelData
					font.family:		jaspTheme.font
					font.pixelSize:		freshAndFunky.font.pixelSize
					Layout.preferredWidth:	560 * welcomeRoot.scaler
					//font.weight:		Font.Thin
					opacity:			0.7
					verticalAlignment:	Text.AlignVCenter
					color:				jaspTheme.black
					wrapMode:			Text.WordWrap
					renderType:			Text.QtRendering
				}
			}

			GridLayout
			{
				id:							bulletPoints
				width:						parent.widthOverflowers
				columns:					3
				rows:						3
				flow:						GridLayout.TopToBottom
				rowSpacing:					8 //* preferencesModel.uiScale
				columnSpacing:				10
				anchors.horizontalCenter:	parent.horizontalCenter
				y:							(parent.height / 2) - (height / 2) - (20 * welcomeRoot.scaler)

				Repeater
				{
					model:		3
					delegate:	orangeDot
				}
				Repeater
				{
					model:		[qsTr("Free:"), qsTr("Friendly:"), qsTr("Flexible:")]
					delegate:	blueKeyword

				}
				Repeater
				{
					model:		[
									qsTr("JASP is an open-source project with structural support from the University of Amsterdam."),
									qsTr("JASP has an intuitive interface that was designed with the user in mind."),
									qsTr("JASP offers standard analysis procedures in both their classical and Bayesian manifestations.")
								]
					delegate:	explanationElement
				}

			}

			Text
			{
				id:						openADataFile
				text:					qsTr("So open a data file and take JASP for a spin!")
				color:					jaspTheme.black
				font.underline:			openDataFileMouse.containsMouse
				font.family:			jaspTheme.font
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
					font.family:			jaspTheme.font
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
				font.family:			jaspTheme.font
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
			height:					sourceSize.height //200  * welcomeRoot.scaler
			sourceSize.width:		1400 * welcomeRoot.scaler
			sourceSize.height:		86 * welcomeRoot.scaler
			source:					jaspTheme.iconPath + "jasp-wave-down-blue-120.svg"
			cache:					false
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
			cache:					false
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
		color:		jaspTheme.jaspBlue
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
		color:		jaspTheme.jaspGreen
		anchors
		{
			top:	parent.verticalCenter
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}
	}

	AnimatedImage
	{
		id:					easterEgg

		property var date:	new Date();

		visible:			date.getMonth() === 11 && date.getDay() > 21 //11 is december and winter starts on the 21st
		playing:			visible
		source:				visible ? "qrc:/html/img/snow.gif" : ""
		fillMode:			Image.TileHorizontally
		z:					8
		anchors
		{
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
		}
	}
}
