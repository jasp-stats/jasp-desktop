import QtQuick 2.7
import QtQuick.Controls 2.3 as New
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3
import "FilterConstructor"

FocusScope
{
	id: filterContainer
    height: filterWindow.minimumHeightTextBoxes * 3
	Layout.minimumHeight: showEasyFilter ? easyFilterConstructor.desiredMinimumHeight : filterWindow.minimumHeightTextBoxes + applyFilter.implicitHeight + (filterErrorScroll.visible ? filterError.contentHeight : 0 ) + filterGeneratedBox.height

    visible: opened


    property bool opened: false
    property int minimumHeightTextBoxes: 50
    property string lastAppliedFilter: defaultFilter
	property bool showEasyFilter: true

	onShowEasyFilterChanged: if(!showEasyFilter) filterEdit.text = engineSync.getFilter()

    function toggle()
    {
        opened = !opened
        filterEdit.text = engineSync.getFilter()

		if(opened)
			height = parent.height / 2
    }

    function open()
    {
        if(!opened)
            toggle();
    }

    function sendFilter()
    {
        engineSync.sendFilter(generatedFilter, lastAppliedFilter)
    }

    function applyAndSendFilter(newFilter)
    {
        lastAppliedFilter = newFilter

        sendFilter()
    }

	function resetFilter()
	{
		filterEdit.text = defaultFilter
		applyAndSendFilter(defaultFilter)
	}

	signal rCodeChanged(string rScript)

	FilterConstructor {
		id: easyFilterConstructor
		anchors.fill: parent
		visible: parent.showEasyFilter
		extraSpaceUnderColumns: gearAdvanced.height

		Image
		{
			id: gearAdvanced
			source: "qrc:/icons/R.png"
			anchors.bottom: parent.bottom
			anchors.left: parent.left
			anchors.margins: 4
			width: 22
			height: width

			sourceSize.width: width
			sourceSize.height: height

			MouseArea
			{
				anchors.fill: parent
				onClicked: filterContainer.showEasyFilter = false
				hoverEnabled: true

				New.ToolTip.delay: 500
				New.ToolTip.visible: containsMouse
				New.ToolTip.text: "Click here to switch to an advanced filter-editor (using R directly)"

				cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			}
		}

		Image
		{
			id: closeEasyFilter
			source: "../images/cross.png"
			anchors.bottom: parent.bottom
			anchors.right: parent.right
			anchors.margins: 4
			width: 22
			height: width

			sourceSize.width: width
			sourceSize.height: height

			MouseArea
			{
				anchors.fill: parent
				onClicked: filterWindow.toggle()
				hoverEnabled: true

				New.ToolTip.delay: 500
				New.ToolTip.visible: containsMouse
				New.ToolTip.text: "Click here to close the filter constructor"

				cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			}
		}

		onRCodeChanged: filterContainer.rCodeChanged(rScript)
	}

    SplitView
    {
		visible: !parent.showEasyFilter
        anchors.fill: parent

        orientation: Qt.Vertical

        Rectangle {
            id: filterEditPlusButton
            color: systemPalette.base

            border.width: 1
            border.color: systemPalette.mid
            Layout.fillHeight: true
            Layout.minimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight

			New.ScrollView
			{
				id: filterScroller
				anchors.top: parent.top
				anchors.left: parent.left
				anchors.right: parent.right
				anchors.bottom: applyFilter.top
				clip: true

				contentWidth: width
				contentHeight: filterGeneratedBox.height + filterEditBox.height

				Item
				{
					id: filterGeneratedBox
					anchors.top: parent.top
					anchors.left: parent.left
					anchors.right: parent.right

					height: filterGeneratedEdit.contentHeight

					New.TextArea
					{
						id: filterGeneratedEdit
						anchors.top: filterGeneratedBox.top
						anchors.left: resetAllGeneratedFilters.right
						anchors.right: filterGeneratedBox.right
						text: generatedFilter +"\n"
						height: contentHeight
						readOnly: true
						color: "gray"
						selectByMouse: true
						onActiveFocusChanged: if(!activeFocus) deselect()
					}

					New.Button
					{
						id: resetAllGeneratedFilters
						anchors.left: parent.left
						anchors.verticalCenter: parent.verticalCenter
						width: dataSetModel.columnsFilteredCount > 0 ? implicitWidth : 0
						icon.source: "../images/eraser_all.png"
						visible: dataSetModel.columnsFilteredCount > 0
						anchors.margins: 1
						onClicked: dataSetModel.resetAllFilters()

						background: Rectangle {	color: "transparent" }
					}
				}

				Item
				{
					//Must be here because otherwise filterEdit turns its clipping on, because it is in a scrollview...
					id: filterEditBox
					anchors.top: filterGeneratedBox.bottom
					anchors.left: parent.left
					anchors.right: parent.right
					height: filterEdit.height

					New.TextArea
					{

						id: filterEdit

						anchors.top: parent.top
						anchors.left: parent.left
						anchors.right: parent.right
						height: contentHeight + 30
						selectByMouse: true
						onActiveFocusChanged: if(!activeFocus) deselect()
					}
				}
			}


			New.Button
			{
				id: easyFilterButton
				//icon.source: "qrc:/icons/NotR.png"
				anchors.left: parent.left
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterContainer.showEasyFilter = true

				width: visible ? height : 0



				MouseArea
				{
					anchors.fill: parent
					New.ToolTip.delay: 500
					New.ToolTip.visible: containsMouse
					New.ToolTip.text: "Click here to switch to the easier filter-constructor"
					acceptedButtons: Qt.NoButton
				}

				background: Rectangle
				{
					color: parent.hovered ? "white" : "lightGrey"
					border.color: parent.hovered ? "black" : "grey"
					border.width: 1

					Image
					{
						source: "qrc:/icons/NotR.png"
						sourceSize.width: width
						sourceSize.height: height

						anchors.fill: parent
						anchors.margins: 6
					}
				}
			}

			New.Button
			{
				id: clearFilterButton
				icon.source: "../images/eraser.png"
				anchors.left: easyFilterButton.right
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterWindow.resetFilter()

				width: visible ? implicitWidth : 0
				height: filterContainer.buttonsHeight
				visible: filterEdit.text != defaultFilter

				background: Rectangle
				{
					color: parent.hovered ? "white" : "lightGrey"
					border.color: parent.hovered ? "black" : "grey"
					border.width: 1
				}
			}

			New.Button
            {
                id: applyFilter
                text: "Apply Filter"
                anchors.left: clearFilterButton.right
                anchors.right: closeFilterButton.left
                anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

                onClicked: filterWindow.applyAndSendFilter(filterEdit.text)

				background: Rectangle
				{
					color: parent.hovered ? "white" : "lightGrey"
					border.color: parent.hovered ? "black" : "grey"
					border.width: 1
				}

            }

			New.Button
            {
                id: closeFilterButton
				icon.source: "../images/cross.png"
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                onClicked: filterWindow.toggle()
			//	height: filterContainer.buttonsHeight

				background: Rectangle
				{
					color: parent.hovered ? "white" : "lightGrey"
					border.color: parent.hovered ? "black" : "grey"
					border.width: 1
				}
            }
        }

		New.ScrollView
		{
			Layout.minimumHeight: filterWindow.minimumHeightTextBoxes//filterError.contentHeight//Math.min(filterError.contentHeight, filterWindow.minimumHeightTextBoxes)
			id: filterErrorScroll

			New.TextArea
			{
				id: filterError
				color: "red"
				readOnly: true
				text: filterErrorText
				selectByMouse: true
				onActiveFocusChanged: if(!activeFocus) deselect()


				states: [
					State {
						name: "closed"
						PropertyChanges { target: filterErrorScroll; visible: false; height: 0 }
						when: filterError.text.length == 0
					},
					State {
						name: "opened"
						PropertyChanges { target: filterErrorScroll; visible: true; height: filterError.contentHeight} //Math.min( , filterWindow.minimumHeightTextBoxes)

						when: filterError.text.length > 0
					}
				]
			}
		}

    }

}
