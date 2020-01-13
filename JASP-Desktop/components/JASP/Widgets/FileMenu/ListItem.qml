import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Controls 1.0


FocusScope
{
	id: rectTitleAndDescripton

	width:			300 //Should be set from ListView
	height:			rectTitle.height + rectDescription.height + 3

	property alias color:  rectTitleBackground.color
	property alias border: rectTitleBackground.border

	Rectangle
	{
		id:				rectTitleBackground
		border.width:	1
		border.color:	allHovered || rectTitleAndDescripton.activeFocus ? jaspTheme.buttonBorderColorHovered : jaspTheme.buttonBorderColor
		color:			rectTitle.color
		z:				-1
		anchors.fill:	parent
	}

	property var cppModel:			undefined

	property bool mainHovered:		descriptionMouseArea.containsMouse || fileEntryMouseArea.containsMouse
	property bool allHovered:		(mainHovered || firstFileOrFolderMouseArea.containsMouse) || datafileMouseArea.containsMouse
	property bool mainPressed:		descriptionMouseArea.pressed || fileEntryMouseArea.pressed
	property bool allPressed:		mainPressed || firstFileOrFolderMouseArea.pressed || datafileMouseArea.pressed
	property bool hasBreadCrumbs:	false

	onAllHoveredChanged:	if(allHovered) { ListView.currentIndex = index; forceActiveFocus(); }

	Keys.onEnterPressed:									openStuff(model);
	Keys.onReturnPressed:									openStuff(model);
	Keys.onSpacePressed:									openStuff(model);
	Keys.onRightPressed:			if(model.type === 3)	openStuff(model);

	function openStuff(model)
	{
		if (!rectTitleAndDescripton.cppModel.mayOpen())	return;

		if (model.type === 3)	rectTitleAndDescripton.cppModel.changePath(model.name, model.path); //Folder type
		else					rectTitleAndDescripton.cppModel.openFile(model.path)
	}

	Rectangle
	{
		id:					rectTitle
		height:				40 * preferencesModel.uiScale
		width:				parent.width
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		parent.top
		anchors.margins:	1

		color:				rectTitleAndDescripton.allPressed || (rectTitleAndDescripton.activeFocus && !datafileMouseArea.containsMouse) ?
								jaspTheme.buttonColorPressed :
								rectTitleAndDescripton.allHovered
								? jaspTheme.buttonColorHovered : jaspTheme.buttonColor

		Image
		{
			id :				firstFileOrFolderImage

			height:					model.type==3 ? 0.75 * rectTitle.height : 0.95 * rectTitle.height  //Tune folder image to file image (wtih topmargin in svg)
			width:					height
			anchors.left:			rectTitle.left
			anchors.verticalCenter: parent.verticalCenter
			anchors.leftMargin:		model.type==3 ? 5 * preferencesModel.uiScale : 0

			fillMode:	Image.PreserveAspectFit
			source:		model.iconsource
			mipmap:		true

			sourceSize
			{
				width:	firstFileOrFolderImage.width * 2
				height:	firstFileOrFolderImage.height * 2
			}

			MouseArea
			{
				id:					firstFileOrFolderMouseArea
				z:					-2
				anchors.fill:		parent
				hoverEnabled:		true
				cursorShape:		Qt.PointingHandCursor
				onClicked:			rectTitleAndDescripton.openStuff(model)
				onDoubleClicked:	{}
			}

			ToolTip
			{
				id:			firstFileOrFolderTooltip
				delay:		500
				text:		commonToolTip.text
				visible:	firstFileOrFolderMouseArea.containsMouse
				font:		jaspTheme.font
			}
		}

		Rectangle
		{
			id:				rectTitleAndDataFile
			height:			textTitle.height
			color:			!hasDatafile ? "transparent" :
										   datafileMouseArea.pressed || (rectTitleAndDescripton.activeFocus && datafileMouseArea.containsMouse) ?
											   jaspTheme.buttonColorPressed :
											   datafileMouseArea.containsMouse || rectTitleAndDescripton.activeFocus ?
												   jaspTheme.buttonColorHovered : jaspTheme.buttonColor
			border.color:	jaspTheme.uiBorder
			border.width:	hasDatafile ? 1 : 0

			property bool hasDatafile: model.associated_datafile !== ""

			anchors
			{
				left:		firstFileOrFolderImage.right
				top:		parent.top
				right:		parent.right
				margins:	1
			}

			Image
			{
				id :			associatedDatafileImage

				height:			0.95 * parent.height
				width:			model.associated_datafile === "" ? 0 : height
				anchors.left:	parent.left
				anchors.top:	parent.top


				fillMode:		Image.PreserveAspectFit
				source:			model.dataiconsource
				visible :		rectTitleAndDataFile.hasDatafile
				mipmap:			true


				sourceSize
				{
					width:	associatedDatafileImage.width * 2
					height:	associatedDatafileImage.height * 2
				}
			}

			MouseArea
			{
				id:				datafileMouseArea
				z:				-2
				anchors.fill:	rectTitleAndDataFile.hasDatafile ? parent : undefined
				hoverEnabled:	true

				onClicked:		rectTitleAndDescripton.cppModel.openFile(model.dirpath + model.associated_datafile)
				cursorShape:	Qt.PointingHandCursor
			}

			ToolTip
			{
				id:			datafileToolTip
				delay:		500
				text:		toolTipText(model.action, model.type, model.associated_datafile, "datafileMouseArea")
				visible:	datafileMouseArea.containsMouse
				font:		jaspTheme.font
			}

			Text
			{
				id:					textTitle

				height:				hasBreadCrumbs ?  rectTitle.height : rectTitle.height / 2
				anchors.top:		parent.top
				anchors.left:		associatedDatafileImage.right
				anchors.right:		parent.right
				anchors.leftMargin:	10 * preferencesModel.uiScale

				text:					model.name
				color:					jaspTheme.textEnabled
				font:					jaspTheme.font
				horizontalAlignment:	Text.AlignLeft
				verticalAlignment:		Text.AlignVCenter
				elide:					Text.ElideMiddle
			}
		}

		Text
		{
			id:						textFolder
			visible:				!hasBreadCrumbs

			height:					hasBreadCrumbs ?  parent.height :parent.height / 2
			anchors.top:			rectTitleAndDataFile.bottom
			anchors.left:			rectTitleAndDataFile.left
			anchors.right:			parent.right
			anchors.leftMargin:		(10 * preferencesModel.uiScale) + associatedDatafileImage.width
			text:					model.dirpath
			color:					jaspTheme.textEnabled
			horizontalAlignment:	Text.AlignLeft
			verticalAlignment:		Text.AlignVCenter
			font:					jaspTheme.font
			elide:					Text.ElideMiddle
		}

		MouseArea
		{
			z:					-1
			id:					fileEntryMouseArea
			anchors.fill:		parent
			hoverEnabled:		true
			cursorShape:		Qt.PointingHandCursor
			onClicked:			rectTitleAndDescripton.openStuff(model)
			onDoubleClicked:	{}
		}

		ToolTip
		{
			id:			commonToolTip
			delay:		500
			text:		toolTipText(model.action, model.type, model.associated_datafile, "commonMouseArea")
			visible:	rectTitleAndDescripton.mainHovered
			font:		jaspTheme.font
		}

	}

	Rectangle
	{
		id: rectDescription

		height:				visible ? Math.max(40,textDescription.contentHeight) + 30 : 1
		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		rectTitle.bottom
		anchors.margins:	visible ? 1 : 0

		color:				rectTitleAndDescripton.color//allHovered ? jaspTheme.buttonColorHovered : jaspTheme.buttonColor
		visible:			model.description !== ""

		Text
		{
			id:	textDescription

			anchors
			{
				left:			parent.left
				right:			parent.right
				top:			parent.top
				leftMargin:		10 * preferencesModel.uiScale
				rightMargin:	10 * preferencesModel.uiScale
				topMargin:		10 * preferencesModel.uiScale
			}

			horizontalAlignment:	Text.AlignJustify
			wrapMode:				Text.WordWrap
			font:					jaspTheme.font
			color:					jaspTheme.textEnabled
			textFormat:				Text.StyledText
			text:					model.description
		}

		MouseArea
		{
			id:					descriptionMouseArea
			anchors.fill:		parent
			hoverEnabled:		true
			onClicked:			rectTitleAndDescripton.openStuff(model)
			onDoubleClicked:	{}
			cursorShape:		Qt.PointingHandCursor
		}
	}

	function toolTipText(action, type, associated_datafile, mousearea)
	{
		if (action === "sync")
			return qsTr("Synchronize with this Data File");

		//model type: JASP = 0, CSV = 1, SPSS = 2, Folder = 3, Other = 4, NoOfTypes = 5

		if (type === 3)
			return qsTr("Navigate to folder")

		if ( (associated_datafile === "" && type === 0) || (associated_datafile !== "" && mousearea === "commonMouseArea") )
			return qsTr("Open JASP file")

		return qsTr("Open data file")

	}
}
