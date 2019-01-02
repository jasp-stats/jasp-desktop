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
import JASP.Controls	1.0
import JASP.Theme		1.0
import JASP.Widgets		1.0
import JASP 1.0

//AnalysisForm
Rectangle
{
	color:			Theme.analysisBackgroundColor
	border
	{
		color:		Theme.buttonBorderColor
		width:		1
	}

	id:				form
	width:			implicitWidth
	implicitWidth:	Theme.formWidth
	implicitHeight: expanderButton.height + (Theme.formMargin * 2)
	height:			implicitHeight



	default property alias	content:			column.children
			property bool	usesJaspResults:	false
			property int	majorVersion:		1
			property int	minorVersion:		0
			property bool	usesVariablesModel: false
			property int	availableWidth:		form.width - 2 * Theme.formMargin
			property var	jaspControls:		[]

            property int    plotHeight:         320
            property int    plotWidth:          480

			property bool	expanded:			currentSelected
			property bool	currentSelected:	analysesModel.currentAnalysisIndex === myIndex


/*	Rectangle
	{
		z:				-1
		anchors.fill:	parent
		color:			Theme.analysisBackgroundColor
		border
		{
			color:		Theme.buttonBorderColor
			width:		1
		}
	}*/


	function getJASPControls(controls, item)
	{
		for (var i = 0; i < item.children.length; ++i)
		{
            var child = item.children[i];

			if (child instanceof ExpanderButton)
			{
                controls.push(child.button);
                getJASPControls(controls, child.area);
			}
			else if (child instanceof JASPControl)
			{
				if (child.hasTabFocus)
                    controls.push(child);
				 else
                    getJASPControls(controls, child);

			}
			else
                getJASPControls(controls, child);
        }            
    }        
     
    IntegerField { visible: false; name: "plotWidth"; value: plotWidth }
    IntegerField { visible: false; name: "plotHeight"; value: plotHeight }    

	Item
	{
		id:					expanderButton
		height:				expanderRectangle.height + formContent.height

		anchors.left:		parent.left
		anchors.right:		parent.right
		anchors.top:		parent.top
		anchors.margins:	Theme.formMargin

		function toggleExpander()
		{
			if(analysesModel.currentAnalysisIndex === myIndex)
				analysesModel.currentAnalysisIndex = -1;
			else
				analysesModel.currentAnalysisIndex = myIndex;

			/*if(listView.currentIndex === myIndex)
				listView.currentIndex = -1
			else
			{
				listView.currentIndex = myIndex
				formContent.forceActiveFocus()
			}*/
		}

		//KeyNavigation.tab: expanderWrapper.expanded ? childControls[0] : nextExpander

		Item
		{
			id:				expanderRectangle
			height:			label.contentHeight + (2 * Theme.formExpanderButtonPadding)

			anchors.left:		parent.left
			anchors.right:		parent.right
			anchors.top:		parent.top


			MouseArea
			{
				id:				mouseArea
				anchors.fill:	parent
				onClicked:		expanderButton.toggleExpander();
				hoverEnabled:	true
				cursorShape:	containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			}

			Image
			{
				id:					icon
				height:				label.contentHeight
				width:				label.contentHeight
				source:				iconsFolder + (expanded ? expandedIcon : contractedIcon)
				sourceSize.width:	width * 2
				sourceSize.height:	height * 2
				anchors
				{
					left:			parent.left
					leftMargin:		6
					verticalCenter:	parent.verticalCenter
				}

				readonly property string iconsFolder:	"qrc:/images/"
				readonly property string expandedIcon:	"expander-arrow-down.png"
				readonly property string contractedIcon: "expander-arrow-up.png"
			}

			Text
			{
				id:			label
				text:		analysisTitle
				font:		Theme.fontLabel
				anchors
				{
					left:			icon.right
					right:			helpButton.left
					margins:		5
					verticalCenter:	parent.verticalCenter
				}
			}

			Image
			{
				// TODO: Make this a button.
				//       Add hover text
				//       Add action: open corresponding help file
				id:					helpButton
				height:				label.contentHeight
				width:				label.contentHeight
				source:				form.expanded ? "qrc:/images/info-button.png" : "qrc:/images/info-button-grey.png"
				// {info-button, info-button-grey}.png Icons made by Freepik from https://www.flaticon.com/
				visible:			mouseArea.containsMouse || form.expanded
				sourceSize.width:	width * 2
				sourceSize.height:	height * 2
				anchors
				{
					right:			closeButton.left
					rightMargin:	6
					verticalCenter:	parent.verticalCenter
				}
			}

			Image
			{
				id:					closeButton
				height:				label.contentHeight
				width:				label.contentHeight
				source:				form.expanded ? "qrc:/images/close-button.png" : "qrc:/images/close-button-grey.png"
				// {close-button, close-button-grey}.png Icons made by Smashicons from https://www.flaticon.com/
				visible:			mouseArea.containsMouse || form.expanded
				sourceSize.width:	width * 2
				sourceSize.height:	height * 2
				anchors
				{
					right:			parent.right
					rightMargin:	6
					verticalCenter:	parent.verticalCenter
				}
			}
		}

		FocusScope
		{
			id:				formContent
			anchors.top:	expanderRectangle.bottom

			width:			parent.width
			height:			!form.expanded ? 0 : errorMessagesBox.height + column.implicitHeight

			clip:			true

			Behavior on height { PropertyAnimation { duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 } }

			Rectangle
			{
				property alias text:	errorMessagesText.text

				id:				errorMessagesBox
				objectName:		"errorMessagesBox"
				visible:		false
				color:			Theme.errorMessagesBackgroundColor
				width:			parent.width
				height:			visible ? errorMessagesText.height : 0

				Text
				{
					id:					errorMessagesText
					anchors.centerIn:	parent
					padding:			5
					verticalAlignment:	Text.AlignVCenter
				}
			}

			ColumnLayout {
				id:				column
				anchors.top:	errorMessagesBox.bottom
				spacing:		10
				width:			parent.width

				//visible:		currentSelected
			}
		}
	}

    Component.onCompleted: {
        var previousExpander = null;
        getJASPControls(jaspControls, column);
        for (var i = 0; i < jaspControls.length; i++) {
            var next = i >= (jaspControls.length-1) ? 0 : i+1;
            if (jaspControls[i].controlType !== "Expander")
                jaspControls[i].KeyNavigation.tab = jaspControls[next];
            else {
                if (previousExpander)
                    previousExpander.nextExpander = jaspControls[i];
                previousExpander = jaspControls[i];
            }
        }

        if (previousExpander)
            previousExpander.nextExpander = jaspControls[0];
        
        for (var i = 0; i < jaspControls.length; i++) {
            if (jaspControls[i].indent)
                jaspControls[i].Layout.leftMargin = Theme.indentationLength
        }        
    }
}
