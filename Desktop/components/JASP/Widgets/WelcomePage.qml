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
import QtQuick.Layouts
import JASP.Widgets

FocusScope
{
	id:		welcomeRoot

	property real scaler: Math.max(0.85, Math.min(2, 0.85 * Math.min(welcomeRoot.width / info.baseWidth, welcomeRoot.height / info.baseHeight)))

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

			
		
			Text
			{
				id:				welcomeToJASP
				text:			! PRO? qsTr("Welcome to JASP").replace(/, /g, ",&nbsp;") : qsTr("Welcome to JASP Enterprise").replace(/, /g, ",&nbsp;")
				color:			jaspTheme.white
				font.family:	jaspTheme.font.family
				font.pixelSize: 30 * welcomeRoot.scaler
				font.weight:	Font.Bold
				renderType:		Text.QtRendering
				textFormat:		Text.StyledText

				anchors
				{
					horizontalCenter:	parent.horizontalCenter
				}
			}
			
			//Text
			//{
			//	id:				scg
			//	text:			company name
			//	color:			jaspTheme.white
			//	font.family:	jaspTheme.font.family
			//	font.pixelSize: 20 * welcomeRoot.scaler
			//	font.weight:	Font.Medium
			//	renderType:		Text.QtRendering
			//	textFormat:		Text.StyledText

			//	anchors
			//	{
			//		horizontalCenter:	parent.horizontalCenter
			//		top:				welcomeToJASP.bottom
			//		topMargin:			height * 2
			//	}
			//}



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
				text:			(!PRO ? qsTr("A Fresh Way to Do Statistics: Free, Friendly, and Flexible") : qsTr("Open-Source Support for Enterprise Success")).replace(/, /g, ",&nbsp;")
				color:			jaspTheme.white
				font.family:	jaspTheme.font.family
				font.pixelSize: 16 * welcomeRoot.scaler
				font.weight:	Font.Normal
				renderType:		Text.QtRendering
				textFormat:		Text.StyledText

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
					font.family:		jaspTheme.font.family
					font.pixelSize:		freshAndFunky.font.pixelSize
					//font.weight:		Font.Bold
					verticalAlignment:	Text.AlignVCenter
					renderType:			Text.QtRendering
					textFormat:			Text.StyledText
					color:				"#23a1df"
				}
			}

			Component
			{
				id:						explanationElement

				Text
				{
					id:						explanationText
					Layout.leftMargin:		10
					Layout.alignment:		Qt.AlignLeft | Qt.AlignTop
					text:					modelData
					font.family:			jaspTheme.font.family
					font.pixelSize:			freshAndFunky.font.pixelSize
					Layout.preferredWidth:	560 * welcomeRoot.scaler
					//font.weight:			Font.Thin
					opacity:				0.7
					verticalAlignment:		Text.AlignVCenter
					color:					jaspTheme.black
					linkColor:				jaspTheme.jaspBlue
					wrapMode:				Text.WordWrap
					renderType:				Text.QtRendering
					textFormat:				Text.StyledText
					onLinkActivated:		mainWindow.showCommunity()
					
					MouseArea
					{
						anchors.fill:		parent
						acceptedButtons:	Qt.NoButton
						cursorShape:		explanationText.hoveredLink !== "" ? Qt.PointingHandCursor : Qt.ArrowCursor
					}
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
					model:		!PRO ? [qsTr("Free:"), qsTr("Friendly:"), qsTr("Flexible:")] : [ "Open:", "Secure:", "Effective:" ]
					delegate:	blueKeyword

				}
				Repeater
				{
					model:		!PRO ?
								[
									qsTr("JASP is an open-source project with structural support from the <a href=\"dummyLink\">University of Amsterdam &amp; others</a>.").replace(/, /g, ",&nbsp;"),
									qsTr("JASP has an intuitive interface that was designed with the user in mind.").replace(/, /g, ",&nbsp;"),
									qsTr("JASP offers standard analysis procedures in both their classical and Bayesian manifestations.").replace(/, /g, ",&nbsp;")
								]
								:
								[ 
										"Our source code will always be open and available", 
										"Sandboxing keeps supply-chain attacks contained",
										"Used world-wide for quality control and statistics" 
								];
					delegate:	explanationElement
				}

			}

			Text
			{
				id:						openADataFile
				text:					(!PRO ? qsTr("So open a data file and take JASP for a spin!") : qsTr("Get started!")).replace(/, /g, ",&nbsp;")
				color:					jaspTheme.blue
				font.underline:			true
				font.family:			jaspTheme.font.family
				font.pixelSize:			freshAndFunky.font.pixelSize + (2 * welcomeRoot.scaler)
				renderType:				Text.QtRendering
				textFormat:				Text.StyledText

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
				visible:			mainWindow.downloadNewJASPUrl !== "" && !PRO
				z: 					1

				anchors
				{
					top:					openADataFile.bottom
					topMargin:				(openADataFile.height * 2) - (height)
					horizontalCenter:		openADataFile.horizontalCenter
				}

				Text
				{
					id:						downloadNewJASP
					anchors.centerIn:		parent
					text:					qsTr("Click to get latest version").replace(/, /g, ",&nbsp;")
					font.family:			jaspTheme.font.family
					font.pixelSize:			openADataFile.font.pixelSize + (downloadMouseArea.containsMouse ? 4 * welcomeRoot.scaler : 0)
					font.weight:			Font.Bold
					color:					jaspTheme.white
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					renderType:				Text.QtRendering
					textFormat:				Text.StyledText
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

			property int widthOverflowers:	!PRO ? width * 0.9 : width * 0.7

			Supporters
			{
				width:					parent.widthOverflowers
				visible:				!PRO

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
			source:					jaspTheme.iconPath + (!PRO ? "jasp-wave-down-blue-120.svg" : "jasp-wave-down-pro-120.svg")
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
			source:					jaspTheme.iconPath + (!PRO ? "jasp-wave-up-green-120.svg" : "jasp-wave-up-pro-120.svg")
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
		property color jaspProBackGroundTop:  "#007f8f"
		
		z:			-1
		color:		!PRO ? jaspTheme.jaspBlue : jaspProBackGroundTop
		anchors
		{
			top:	parent.top
			left:	parent.left
			right:	parent.right
			bottom:	parent.verticalCenter
		}
		
		Image
		{
			id:				jaspLogo
			source:			jaspTheme.iconPath + (!PRO ? "jasp-logo.svg" : "jasp-logo-pro.svg")
			width:			(190 / 40) * height
			height:			info.height / 14
			mipmap:			true
			sourceSize
			{
				width:		jaspLogo.width  * 2
				height:		jaspLogo.height * 2
			}
			scale:			logoMouse.containsMouse ? 1.05 : 1

			anchors
			{
				top:		parent.top
				left:		parent.left
				margins:	jaspUrl.anchors.margins
			}
			
			MouseArea
			{
				id:						logoMouse
				hoverEnabled:			true
				onClicked:				Qt.openUrlExternally(jaspStatsMouseArea.websiteUrl);
				anchors.fill:			parent
				cursorShape:			Qt.PointingHandCursor
			}
		}
		
		Text
		{
			id:				version
			text:			mainWindow.versionString()
			color:			jaspTheme.white
			font.family:	jaspTheme.font.family
			font.pixelSize: 14 * welcomeRoot.scaler
			font.weight:	Font.Normal
			font.underline: versionMouseArea.containsMouse
			renderType:		Text.QtRendering

			anchors
			{
				top:		parent.top
				right:		parent.right
				margins:	jaspUrl.anchors.margins
			}
			
			MouseArea
			{
				id:						versionMouseArea
				hoverEnabled:			true
				onClicked:				Qt.openUrlExternally(aboutModel.commitUrl);
				anchors.fill:			parent
				cursorShape:			Qt.PointingHandCursor
			}
		}
	}

	Rectangle
	{
		id:			greenBackgroundTop
		z:			-1
		property color jaspProBackGroundBottom: "#FF007F8F"
		color:		!PRO ? jaspTheme.jaspGreen : jaspProBackGroundBottom
		anchors
		{
			top:	parent.verticalCenter
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}
		
		Text
		{
			id:						jaspUrlBug
			text:					qsTr("Report bugs")
			color:					jaspTheme.white
			font.family:			jaspTheme.font.family
			font.pixelSize:			14 * welcomeRoot.scaler
			font.weight:			Font.Normal
			font.underline:			bugsMouseArea.containsMouse
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignRight

			anchors
			{
				bottom:				jaspUrlFeatures.top
				left:				parent.left
				leftMargin:			10 * welcomeRoot.scaler
				bottomMargin:		4 * welcomeRoot.scaler
			}
			
			JASPMouseAreaToolTipped
			{
				id:						bugsMouseArea
				hoverEnabled:			true
				onClicked:				Qt.openUrlExternally(mainWindow.contactUrlBugs);
				anchors.fill:			parent
				cursorShape:			Qt.PointingHandCursor
				toolTipText:			mainWindow.contactUrlBugs
			}
		}
		
		
		Text
		{
			id:						jaspUrlFeatures
			text:					qsTr("Request features")
			color:					jaspTheme.white
			font.family:			jaspTheme.font.family
			font.pixelSize:			jaspUrlBug.font.pixelSize
			font.weight:			Font.Normal
			font.underline:			featureMouseArea.containsMouse
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignRight

			anchors
			{
				left:				parent.left
				bottom:				parent.bottom
				margins:			10 * welcomeRoot.scaler
			}
			
			JASPMouseAreaToolTipped
			{
				id:						featureMouseArea
				hoverEnabled:			true
				onClicked:				Qt.openUrlExternally(mainWindow.contactUrlFeatures);
				anchors.fill:			parent
				cursorShape:			Qt.PointingHandCursor
				toolTipText:			mainWindow.contactUrlFeatures
			}
		}
		
		Text
		{
			id:						jaspUrlQuestions
			text:					qsTr("Ask a question")
			color:					jaspTheme.white
			font.family:			jaspTheme.font.family
			font.pixelSize:			jaspUrlBug.font.pixelSize
			font.weight:			Font.Normal
			font.underline:			jaspQuestionsMouseArea.containsMouse
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignRight

			anchors
			{
				right:				parent.right
				bottom:				jaspUrl.top
				rightMargin:		10 * welcomeRoot.scaler
				bottomMargin:		4 * welcomeRoot.scaler
			}
			
			JASPMouseAreaToolTipped
			{
				id:						jaspQuestionsMouseArea
				hoverEnabled:			true
				onClicked:				Qt.openUrlExternally(mainWindow.questionsUrl);
				anchors.fill:			parent
				cursorShape:			Qt.PointingHandCursor
				toolTipText:			mainWindow.questionsUrl
			}
		}
		
		Text
		{
			id:						jaspUrl
			text:					qsTr("Visit the website")
			color:					jaspTheme.white
			font.family:			jaspTheme.font.family
			font.pixelSize:			jaspUrlBug.font.pixelSize
			font.weight:			Font.Normal
			font.underline:			jaspStatsMouseArea.containsMouse
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignRight

			anchors
			{
				right:				parent.right
				bottom:				PRO ? parent.bottom : jaspUrlBv.top
				margins:			10 * welcomeRoot.scaler
				bottomMargin:		(PRO ? 10 : 4) * welcomeRoot.scaler
			}
			
			JASPMouseAreaToolTipped
			{
				id:							jaspStatsMouseArea
				hoverEnabled:				true
				property url websiteUrl:	!PRO ? "https://www.jasp-stats.org" : "https://www.jasp-services.com"
				onClicked:					Qt.openUrlExternally(websiteUrl);
				anchors.fill:				parent
				cursorShape:				Qt.PointingHandCursor
				toolTipText:				websiteUrl
			}
		}
		
		Text
		{
			id:						jaspUrlBv
			text:					qsTr("Enterprise Support")
			color:					jaspTheme.white
			font.family:			jaspTheme.font.family
			font.pixelSize:			jaspUrlBug.font.pixelSize
			font.weight:			Font.Normal
			font.underline:			jaspStatsMouseArea2.containsMouse
			renderType:				Text.QtRendering
			textFormat:				Text.StyledText
			horizontalAlignment:	Text.AlignRight
			visible:				!PRO	

			anchors
			{
				right:				parent.right
				bottom:				parent.bottom
				margins:			10 * welcomeRoot.scaler
			}
			
			JASPMouseAreaToolTipped
			{
				id:							jaspStatsMouseArea2
				hoverEnabled:				true
				property url websiteUrl:	"https://www.jasp-services.com"
				onClicked:					Qt.openUrlExternally(websiteUrl);
				anchors.fill:				parent
				cursorShape:				Qt.PointingHandCursor
				toolTipText:				websiteUrl
				visible:					!PRO
			}
		}
		

	}

	AnimatedImage
	{
		id:					easterEgg

		property var date:	new Date();

		visible:			!PRO && date.getMonth() === 11 && date.getDay() > 21 //11 is december and winter starts on the 21st
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
