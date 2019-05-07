import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Controls	1.0
import JASP.Theme		1.0

Rectangle
{
	id:					expanderButton
	height:				loaderAndError.y
	//width:				Theme.formWidth
	anchors.left:		parent.left
	anchors.right:		parent.right
	clip:				true

	color:				Theme.uiBackground
	border.color:		Theme.buttonBorderColor
	border.width:		1

	property alias		myIndex:			loader.myIndex
	property alias		myID:				loader.myID
	property alias		myAnalysis:         loader.myAnalysis
	property string		formQmlUrl:			undefined
	property bool		expanded:			analysesModel.currentAnalysisIndex === myIndex
	property bool		imploded:			height == loader.y

    ToolTip.toolTip.background: Rectangle { color:	Theme.tooltipBackgroundColor } //This does set it for ALL tooltips ever after

	function toggleExpander()
	{
		if(analysesModel.currentAnalysisIndex === myIndex)	analysesModel.unselectAnalysis()
		else												analysesModel.selectAnalysisAtRow(myIndex);
	}
	
	Component.onCompleted: myAnalysis.expandAnalysis.connect(toggleExpander)

	states: [
		State
		{
			name: "expanded";	when: expanderButton.expanded
			PropertyChanges {	target: expanderButton;		height: loaderAndError.y + loaderAndError.height;		}
			PropertyChanges {	target: expanderIcon;		rotation: 90;											}
		}
	]

	transitions: Transition
	{
		NumberAnimation		{ property: "height";	duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 }
		RotationAnimation	{						duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 }
	}
	
	Item
	{
		id:				expanderRectangle
		height:			Theme.formExpanderHeaderHeight  //label.contentHeight

		anchors
		{
			left:		parent.left
			right:		parent.right
			top:		parent.top
			topMargin:	Theme.formMargin
		}


		MouseArea
		{
			id:				mouseArea
			anchors.fill:	parent
			onClicked:		expanderButton.toggleExpander();
			hoverEnabled:	true
			cursorShape:	Qt.PointingHandCursor
		}

		Image
		{
			id:						expanderIcon
			anchors
			{
				left:			parent.left
				leftMargin:		10 * preferencesModel.uiScale
				verticalCenter:	parent.verticalCenter
			}
			height:			analysisTitle.height * 0.88 //expanderRectangle.height / 1.5
			width:			height
			source:			"qrc:/icons/large-arrow-right.png"
			sourceSize
			{
				width:	expanderIcon.width * 2
				height:	expanderIcon.height * 2
			}

		}

		Item
		{
			id:			analysisTitleItem
			height:		analysisTitle.height

			anchors
			{
				left:			expanderIcon.right
				right:			editButton.left
				leftMargin:		expanderIcon.anchors.leftMargin
				verticalCenter:	parent.verticalCenter
			}

			Text
			{
				id:				analysisTitle
				text:			expanderButton.myAnalysis != null ? expanderButton.myAnalysis.title : "?";
				font:			Theme.fontLabel
				visible:		!analysisTitleInput.visible

				anchors
				{
					left:			parent.left
					right:			parent.right
					verticalCenter:	parent.verticalCenter
				}
			}

			TextInput
			{
				id:					analysisTitleInput
				font:				Theme.fontLabel
				visible:			false
				selectByMouse:		true
				color:				Theme.grayDarker

				anchors
				{
					left:			parent.left
					right:			parent.right
					verticalCenter:	parent.verticalCenter
				}


				Keys.onEscapePressed: 	stopEditing(false);
				Keys.onEnterPressed:	stopEditing(true);
				Keys.onReturnPressed: 	stopEditing(true);
				onActiveFocusChanged:	if(!activeFocus && visible)	stopEditing(true);

				function startEditing()
				{
					text	= analysisTitle.text;
					visible = true;

					forceActiveFocus();
				}

				function stopEditing(storeChangedValue)
				{
					if(storeChangedValue && expanderButton.myAnalysis != null)
						expanderButton.myAnalysis.title = text;

					visible = false;
				}
			}
		}

		MenuButton
		{
			id:					editButton
			width:				height
			iconSource:			"qrc:/icons/edit-pencil.png" // Icon made by Chanut from https://www.flaticon.com/
			enabled:			expanderButton.expanded
			onClicked:			analysisTitleInput.startEditing();
			toolTip:			"Edit the title of this analysis"
			radius:				height
			opacity:			enabled ? 1 : 0.5
			anchors
			{
				top:		parent.top
				right:		helpButton.left
				bottom:		parent.bottom
				margins:	4 * preferencesModel.uiScale
			}
		}

		MenuButton
		{
			id:					helpButton
			width:				height
			iconSource:			enabled ? "qrc:/images/info-button.png" : "qrc:/images/info-button-grey.png" // {info-button, info-button-grey}.png Icons made by Freepik from https://www.flaticon.com/
			//visible:			expanderButton.expanded || hovered || mouseArea.containsMouse
			enabled:			expanderButton.expanded
			onClicked:			helpModel.showOrTogglePage(expanderButton.myAnalysis.helpFile)
			toolTip:			"Show info for analysis"
			radius:				height
			anchors
			{
				top:		parent.top
				right:		closeButton.left
				bottom:		parent.bottom
				margins:	editButton.anchors.margins
			}
		}

		MenuButton
		{
			id:					closeButton
			width:				height
			iconSource:			enabled ? "qrc:/images/close-button.png" : "qrc:/images/close-button-grey.png" // {close-button, close-button-grey}.png Icons made by Smashicons from https://www.flaticon.com/
			//visible:			expanderButton.expanded || hovered || mouseArea.containsMouse
			enabled:			expanderButton.expanded
			onClicked:			analysesModel.removeAnalysis(expanderButton.myAnalysis)
			toolTip:			"Remove analysis"
			radius:				height
			anchors
			{
				top:		parent.top
				right:		parent.right
				bottom:		parent.bottom
				margins:	editButton.anchors.margins
			}
		}
	}


	Item
	{
		id:		loaderAndError
		height:	Math.max(loader.height, errorRect.height * preferencesModel.uiScale)

		anchors
		{
			top:				expanderRectangle.bottom
			left:				parent.left
			right:				parent.right
			margins:			Theme.formMargin
		}

		Rectangle
		{
			id: errorRect
			visible:		loader.status === Loader.Error
			anchors.top:	parent.top			
			color:			Theme.errorMessagesBackgroundColor
			width:			parent.width
			height:			visible ? errorMessagesText.height : 0

			Text
			{
				id:					errorMessagesText
				anchors.centerIn:	parent
				width:				parent.width
				padding:			5
				verticalAlignment:	Text.AlignVCenter
				text: loader.status === Loader.Error ? loader.sourceComponent.errorString() : ""
				wrapMode: Text.Wrap
			}
		}

		Loader
		{
			id:					loader
			source:				!expanderButton.imploded || expanderButton.expanded ? formQmlUrl : ""
			asynchronous:		false // makes it slow when true

			anchors
			{
				top:			errorRect.bottom
				left:			parent.left
				right:			parent.right
			}
	
			property int		myIndex:			-1
			property int		myID:				-1
			property var		myAnalysis:         null
		}
	}
}
