import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Controls	1.0
import JASP.Theme		1.0

Rectangle
{
	id:					expanderButton
	height:				loaderAndError.y
	width:				Theme.formWidth
	clip:				true

	color:				Theme.uiBackground
	border.color:		Theme.buttonBorderColor
	border.width:		1

	property alias		myIndex:			loader.myIndex
	property alias		myID:				loader.myID
	property alias		analysisTitle:		loader.analysisTitle
	property alias		myAnalysis:         loader.myAnalysis
	property string		formQmlUrl:			undefined
	property bool		expanded:			analysesModel.currentAnalysisIndex === myIndex
	property bool		imploded:			height == loader.y

	function toggleExpander()
	{
		if(analysesModel.currentAnalysisIndex === myIndex)	analysesModel.unselectAnalysis()
		else												analysesModel.selectAnalysisAtRow(myIndex);
	}

	states: [
		State {
			name: "expanded"
			when: expanderButton.expanded
			PropertyChanges {
				target: expanderButton
				height: loaderAndError.y + loaderAndError.height
			}
			PropertyChanges {
				target: expanderIcon	
				rotation: 90
			}
		}
	]

	transitions: Transition {
		NumberAnimation { property: "height"; duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 }
		RotationAnimation { duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 }
	}
	
	Item
	{
		id:				expanderRectangle
		height:			Theme.formExpanderHeaderHeight  //label.contentHeight

		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		parent.top
		anchors.topMargin:	Theme.formMargin


		MouseArea
		{
			id:				mouseArea
			anchors.fill:	parent
			onClicked:		expanderButton.toggleExpander();
			hoverEnabled:	true
			cursorShape:	Qt.PointingHandCursor
		}

		Canvas
		{
			id:				expanderIcon
			anchors
			{
				left:			parent.left
				leftMargin:		10 * preferencesModel.uiScale
				verticalCenter:	parent.verticalCenter
			}
			height:			expanderRectangle.height / 3.5
			width:			height
			contextType:	"2d"	
			onPaint:
			{
				context.reset();
				context.moveTo(0, 0);
				context.lineTo(width, height/2);
				context.lineTo(0, height);
				context.closePath();
				context.fillStyle = Theme.grayDarker;
				context.fill();
			}
		}
		
		Text
		{
			id:			label
			text:		analysisTitle
			font:		Theme.fontLabel
			anchors
			{
				left:			expanderIcon.right
				right:			helpButton.left
				leftMargin:		10 * preferencesModel.uiScale
				verticalCenter:	parent.verticalCenter
			}
		}

		MenuButton
		{
			id:					helpButton
			width:				height
			iconSource:			enabled ? "qrc:/images/info-button.png" : "qrc:/images/info-button-grey.png" // {info-button, info-button-grey}.png Icons made by Freepik from https://www.flaticon.com/
			//visible:			expanderButton.expanded || hovered || mouseArea.containsMouse
			enabled:			expanderButton.expanded
			onClicked:			helpModel.showOrTogglePage("analyses/" + expanderButton.myAnalysis.name)
			toolTip:			"Show info for analysis"
			radius:				height
			anchors
			{
				top:		parent.top
				right:		closeButton.left
				bottom:		parent.bottom
				margins:	6 * preferencesModel.uiScale
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
				margins:	6 * preferencesModel.uiScale
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
	
			property int		myIndex:			-1
			property int		myID:				-1
			property string		analysisTitle:		"???"
			property var		myAnalysis:         null
		}
	}
}
