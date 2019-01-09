import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Controls 1.0
import JASP.Theme 1.0

Rectangle
{
	id: rectTitleAndDescripton

	width:			300 //Should be set from ListView
	height:			rectTitle.height + rectDescription.height + 3
	border.width:	1
	border.color:	rectTitleAndDescripton.allHovered ? Theme.buttonBorderColorHovered : Theme.buttonBorderColor
	color:			rectTitle.color

	property var cppModel: undefined

	property bool mainHovered:	descriptionMouseArea.containsMouse || fileEntryMouseArea.containsMouse
	property bool allHovered:	mainHovered || firstFileOrFolderMouseArea.containsMouse || datafileMouseArea.containsMouse
	property bool hasBreadCrumbs: false

	function openStuff(model, wasDoubleClick)
	{
		if (model.type === 3 && !wasDoubleClick)
			rectTitleAndDescripton.cppModel.changePath(model.name, model.path); //Folder type

		if (model.type !== 3 && wasDoubleClick)
			rectTitleAndDescripton.cppModel.openFile(model.path)
	}

	Rectangle {

		id:					rectTitle

		height:				40
		width:				parent.width
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		parent.top
		anchors.margins:	1

		color:				rectTitleAndDescripton.allHovered ? Theme.buttonColorHovered : Theme.buttonColor

		Image {
			id :				firstFileOrFolderImage

			height:				0.95 * parent.height
			width:				height
			anchors.left:		rectTitle.left
			anchors.top:		rectTitle.top
			anchors.leftMargin: 10

			fillMode:	Image.PreserveAspectFit
			source:		model.iconsource

			MouseArea
			{
				id:					firstFileOrFolderMouseArea
				z:					-2
				anchors.fill:		parent
				hoverEnabled:		true
				cursorShape:		Qt.PointingHandCursor
				onDoubleClicked:	rectTitleAndDescripton.openStuff(model,true)
				onClicked:			rectTitleAndDescripton.openStuff(model,false)

			}

			ToolTip {
				id:			firstFileOrFolderTooltip
				delay:		500
				text:		commonToolTip.text
				visible:	firstFileOrFolderMouseArea.containsMouse
			}
		}

		Image {
			id :			associatedDatafileImage

			height:			0.95 * parent.height
			width:			model.associated_datafile === "" ? 0 : height
			anchors.left:	firstFileOrFolderImage.right
			anchors.top:	rectTitle.top


			fillMode:		Image.PreserveAspectFit
			source:			model.dataiconsource
			visible :		model.associated_datafile !== ""

			MouseArea {
				z:-2
				id:				datafileMouseArea
				anchors.fill:	parent
				hoverEnabled:	true

				onDoubleClicked:	rectTitleAndDescripton.cppModel.openFile(model.dirpath + model.associated_datafile)
				cursorShape:		Qt.PointingHandCursor
			}

			ToolTip {
				id:			datafileToolTip
				delay:		500
				text:		toolTipText(model.type, model.associated_datafile, "datafileMouseArea")
				visible:	datafileMouseArea.containsMouse
			}
		}

		Text {
			id:					textTitle

			height:				hasBreadCrumbs ?  parent.height : parent.height / 2
			anchors.top:		parent.top
			anchors.left:		associatedDatafileImage.right
			anchors.right:		parent.right
			anchors.leftMargin:	10

			text:					model.name  //i.e. title
			horizontalAlignment:	Text.AlignLeft
			verticalAlignment:		Text.AlignVCenter
		}
		Text {
			id:					textFolder
			visible: !hasBreadCrumbs

			height:				hasBreadCrumbs ?  parent.height :parent.height / 2
			anchors.top:		textTitle.bottom
			anchors.left:		associatedDatafileImage.right
			anchors.right:		parent.right
			anchors.leftMargin:	10
			text:					model.dirpath  //i.e. title
			horizontalAlignment:	Text.AlignLeft
			verticalAlignment:		Text.AlignVCenter
			font.pixelSize: 10
		}
		MouseArea {
			z:					-1
			id:					fileEntryMouseArea
			anchors.fill:		parent
			hoverEnabled:		true
			onDoubleClicked:	rectTitleAndDescripton.openStuff(model,true)
			cursorShape:		Qt.PointingHandCursor
			onClicked:			{
				rectTitleAndDescripton.openStuff(model,false)
			}
		}

		ToolTip {
			id:			commonToolTip
			delay:		500
			text:		toolTipText(model.type, model.associated_datafile, "commonMouseArea")
			visible:	rectTitleAndDescripton.mainHovered
		}

	}

	Rectangle {

		id: rectDescription

		height:				visible ? Math.max(40,textDescription.contentHeight) + 30 : 0
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		rectTitle.bottom
		anchors.margins:	height > 0 ? 1 : 0

		color:				rectTitleAndDescripton.allHovered ? Theme.white : Theme.uiBackground
		visible:			model.description !== ""

		Text {
			id:	textDescription

			anchors
			{
				left:			parent.left
				right:			parent.right
				top:			parent.top
				leftMargin:		10
				rightMargin:	10
				topMargin:		10
			}

			horizontalAlignment:	Text.AlignJustify
			wrapMode:				Text.WordWrap
			font:					Theme.font
			textFormat:				Text.StyledText
			text:					model.description
		}

		MouseArea {
			id:					descriptionMouseArea
			anchors.fill:		parent
			hoverEnabled:		true
			onDoubleClicked:	rectTitleAndDescripton.openStuff(model,true)
			onClicked:			rectTitleAndDescripton.openStuff(model,false)
			cursorShape:		Qt.PointingHandCursor
		}
	}

	function toolTipText(type, associated_datafile, mousearea)
	{
		//model type: JASP = 0, CSV = 1, SPSS = 2, Folder = 3, Other = 4, NoOfTypes = 5

		if (type === 3)
			return "Press to navigate to folder"

		if ( (associated_datafile === "" && type === 0) || (associated_datafile !== "" && mousearea === "commonMouseArea") )
			return "Double click to open JASP file"

		return "Double click to open data file"

	}
}
