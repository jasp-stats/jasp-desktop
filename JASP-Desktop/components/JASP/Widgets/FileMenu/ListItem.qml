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
	border.color:	Theme.grayDarker
	color:			Theme.grayMuchLighter

	property var cppModel: undefined

	function openStuff()
	{
		if (model.type === 3)	rectTitleAndDescripton.cppModel.changePath(model.name, model.path); //Folder type
		else					rectTitleAndDescripton.cppModel.openFile(model.path)
	}

	Rectangle {

		id:					rectTitle

		height:				40
		width:				parent.width
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		parent.top
		anchors.margins:	1

		color:	fileEntryMouseArea.containsMouse ?  Theme.grayDarker : "#dcdadb"

		Image {
			id :				firstFileOrFolderImage

			height:				0.95 * parent.height
			width:				height
			anchors.left:		model.type	===	3 ? rectTitle.left : undefined
			anchors.right:		model.type	!==	3 ? associatedDatafileImage.left : undefined
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
				cursorShape:		containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
				onDoubleClicked:	openStuff()

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
			anchors.right:	parent.right
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
				cursorShape:		containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			}

			ToolTip {
				id:			datafileToolTip
				delay:		500
				text:		toolTipText(model.type, model.associated_datafile, "datafileMouseArea")
				visible:	datafileMouseArea.containsMouse
			}
		}

		Text {
			id:				textTitle

			height:				parent.height
			anchors.top:		parent.top
			anchors.left:		model.type === 3 ? firstFileOrFolderImage.right : parent.left
			anchors.right:		parent.right
			anchors.leftMargin:	10

			text:					model.name  //i.e. title
			horizontalAlignment:	Text.AlignLeft
			verticalAlignment:		Text.AlignVCenter
		}

		MouseArea {
			z:					-1
			id:					fileEntryMouseArea
			anchors.fill:		parent
			hoverEnabled:		true
			onDoubleClicked:	rectTitleAndDescripton.openStuff();
			cursorShape:		containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor

		}

		ToolTip {
			id:			commonToolTip
			delay:		500
			text:		toolTipText(model.type, model.associated_datafile, "commonMouseArea")
			visible:	fileEntryMouseArea.containsMouse
		}

	}

	Rectangle {

		id: rectDescription

		height:				visible ? Math.max(40,textDescription.contentHeight) + 30 : 0
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		rectTitle.bottom
		anchors.margins:	1

		color:				Theme.grayMuchLighter
		visible:			model.description !== ""

		Text {
			id:	textDescription

			anchors
			{
				left:			parent.left
				right:			parent.right
				top:			parent.top
				leftMargin:		10
				topMargin:		10
			}

			horizontalAlignment:	Text.AlignJustify
			wrapMode:				Text.WordWrap
			font:					Theme.font
			textFormat:				Text.StyledText
			text:					model.description
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
