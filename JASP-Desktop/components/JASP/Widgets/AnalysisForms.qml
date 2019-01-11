import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Widgets 1.0
import JASP.Theme 1.0
import QtQuick.Dialogs 1.2


FocusScope
{
	id:			analysisFormsFocusScope
	width:		extraSpace + (mainWindow.analysesVisible ? Theme.formWidth + 1 + (2 * formsBackground.border.width) : 0)

	property int	extraSpace:	analysesModel.count > 0 ? openCloseButton.width : 0

	Behavior on width { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }

	Rectangle
	{
		id:				formsBackground
		z:				0
		color:			Theme.uiBackground
		border.color:	Theme.uiBorder
		border.width:	1
		//visible:		analyses.count > 0
		anchors.fill:	parent

		Item
		{
			anchors.centerIn: parent
			width: messageDialog.width
			height: messageDialog.height
			MessageDialog
			{
				id: messageDialog
				title: "Error"
			}
		}
	
		Item
		{
			id:				dropShadow
			x:				1 - width
			z:				-1
			height:			parent.height
			width:			Theme.shadowRadius

			Rectangle
			{
				anchors.centerIn: parent
				rotation:	90
				gradient:	Gradient {	GradientStop { position: 0.0; color: Theme.shadow }	GradientStop { position: 1.0; color: "transparent" } }
				height:		dropShadow.width
				width:		dropShadow.height
			}
		}

		Rectangle
		{
			id:				openCloseButton
			width:			20 * ppiScale
			color:			Theme.uiBackground
			border.color:	Theme.uiBorder
			border.width:	1
			anchors
			{
				top:	parent.top
				left:	parent.left
				bottom:	parent.bottom
			}

			Image
			{

				readonly property string iconsFolder:		"qrc:/images/"
				readonly property string expandedIcon:		"arrow-right.png"
				readonly property string contractedIcon:	"arrow-left.png"

				source:				iconsFolder + (mainWindow.analysesVisible ? expandedIcon : contractedIcon)
				width:				parent.width - 4
				height:				width
				sourceSize.width:	width * 2;
				sourceSize.height:	height * 2;

				anchors.centerIn:	parent
			}

			MouseArea
			{
				anchors.fill:	parent
				hoverEnabled:	true
				cursorShape:	Qt.PointingHandCursor
				onClicked:		mainWindow.analysesVisible = !mainWindow.analysesVisible
				z:				3
			}
		}


		ScrollView
		{
			z:								2
			visible:						analysisFormsFocusScope.width > analysisFormsFocusScope.extraSpace

			anchors
			{
				top:		parent.top
				left:		openCloseButton.right
				right:		parent.right
				bottom:		parent.bottom
				margins:	parent.border.width
			}

			Column
			{
				width:			Theme.formWidth
				spacing:		1


				Repeater
				{
					model:							analysesModel
					//highlightFollowsCurrentItem:	true

					delegate: Loader
					{
						id: loader
						source:			formPath
						asynchronous:	true

						//property bool	currentSelected:	ListView.isCurrentItem
						//property Item	listView:			ListView.view
						property int	myIndex:			index
						property string	analysisTitle:		name
                        property var    myAnalysis:         analysis
						
						onStatusChanged :
							if (loader.status == Loader.Error)
							{
								messageDialog.text = sourceComponent.errorString();
								messageDialog.visible = true;
							}
					}
				}
			}
		}
	}
}
