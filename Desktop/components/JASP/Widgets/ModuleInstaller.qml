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

import QtQuick					2.11
import QtQuick.Controls			2.7 as QTCONTROLS
import Qt.labs.folderlistmodel	2.11
//import QtQuick.Window			2.3
import JASP.Controls			1.0
import JASP.Widgets				1.0

QTCONTROLS.Popup
{
	id:				moduleInstallerPopup

	y:				(parent.height / 2) - (height / 2)
	x:				(parent.width / 2) - (width / 2)
	width:			900
	height:			500

	modal:			true

	background:		Rectangle { color: jaspTheme.uiBackground }
	closePolicy:	QTCONTROLS.Popup.CloseOnPressOutside | QTCONTROLS.Popup.CloseOnEscape

	Loader
	{
		visible:			moduleInstallerPopup.opened
		sourceComponent:	visible ? moduleInstallerComponent : null
		anchors.fill:		parent
	}

	Component
	{
		id:		moduleInstallerComponent

		Item
		{
			id:			moduleInstallerRect

			RectangularButton
			{
				id:					browseButton
				iconSource:			jaspTheme.iconPath + "folder.svg"
				showIconAndText:	true
				width:				height
				height:				installButton.height
				text:				qsTr("Browse for JASP Module");
				anchors
				{
					top:		parent.top
					left:		parent.left
					right:		closeButtonCross.left
					margins:	jaspTheme.generalAnchorMargin
				}

				function browse() { return messages.browseOpenFileDocumentsQML(qsTr("Select a JASP Module"), "*.tar.gz"); }


				onClicked:		descriptionViewer.currentlySelectedFilePath = browse();
			}

			RectangularButton
			{
				id:				closeButtonCross
				iconSource:		jaspTheme.iconPath + "cross.png"
				width:			height
				height:			installButton.height
				anchors
				{
					top:		parent.top
					right:		parent.right
					margins:	jaspTheme.generalAnchorMargin
				}
				onClicked:	moduleInstallerPopup.close()
			}

			Item
			{
				id:					fileBrowseArea
				anchors.top:		browseButton.bottom
				anchors.left:		parent.left
				anchors.right:		parent.right
				anchors.bottom:		installButton.top

				onHeightChanged:
				{
					if(height <= jaspTheme.generalAnchorMargin * 2)
						visible = false
				}

				Rectangle
				{
					anchors.fill:		parent
					anchors.margins:	jaspTheme.generalAnchorMargin

					color:				jaspTheme.whiteBroken
					border.color:		jaspTheme.gray
					border.width:		1


					Text
					{
						id:					descriptionViewer
						anchors.fill:		parent
						anchors.margins:	jaspTheme.generalAnchorMargin
						color:				jaspTheme.textEnabled
						text:				defaultText
						textFormat:			Text.StyledText
						wrapMode:			Text.WrapAtWordBoundaryOrAnywhere
						onLinkActivated:	Qt.openUrlExternally(link)
						clip:				true

						property string currentlySelectedFilePath:	browseButton.browse()
						property string defaultText:				qsTr("<i>Browse for a JASP Module to see more information here</i>")
						property bool	moduleIsOK:					false

						onCurrentlySelectedFilePathChanged: if(currentlySelectedFilePath !== "") showDescription();

						function showDescription()
						{
							var filePath = currentlySelectedFilePath;

							moduleIsOK = false;

							if(filePath === "")
							{
								descriptionViewer.text = descriptionViewer.defaultText;
								return;
							}

							if(dynamicModules.isFileAnArchive(filePath))
							{
								var loadedQML = dynamicModules.getDescriptionFormattedFromArchive(filePath)

								if(loadedQML !== "")
								{
									moduleIsOK = true;
									descriptionViewer.text = loadedQML
								}

							}

							if(!moduleIsOK)
							{
								descriptionViewer.text = qsTr("<i>%1 is not a (correct) JASP Module!</i>").arg(filePath)
								descriptionViewer.currentlySelectedFilePath = "";
							}
						}

						MouseArea
						{
							anchors.fill:		parent
							acceptedButtons:	Qt.NoButton
							cursorShape:		descriptionViewer.hoveredLink !== "" ? Qt.PointingHandCursor : Qt.ArrowCursor
						}
					}
				}
			}

			RectangularButton
			{
				id:					installButton
				anchors
				{
					left:		parent.left
					right:		parent.right
					bottom:		parent.bottom
					margins:	jaspTheme.generalAnchorMargin
				}
				enabled:			descriptionViewer.moduleIsOK

				text:				"Install"
				toolTip:			enabled ? "Press this to install your selected Module" : "Select a JASP Module to install"

				onClicked:
				{
					if(descriptionViewer.moduleIsOK)
					{
						enabled							= false
						selected						= true

						dynamicModules.installJASPModule(descriptionViewer.currentlySelectedFilePath)

						moduleInstallerPopup.close() //There is no information being shown anyway so lets just close the window now
					}
				}
			}
		}
	}
}
