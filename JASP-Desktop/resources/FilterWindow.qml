import QtQuick 2.7
import QtQuick.Controls 2.3 as New
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3
import "FilterConstructor"

FocusScope
{
	id: filterContainer
    height: filterWindow.minimumHeightTextBoxes * 3
	Layout.minimumHeight: showEasyFilter ? easyFilterConstructor.desiredMinimumHeight : filterWindow.minimumHeightTextBoxes + applyFilter.implicitHeight + (filterError.visible ? filterError.contentHeight : 0 ) + filterGeneratedBox.height

    visible: opened

    property bool opened: false
    property int minimumHeightTextBoxes: 50
    property string lastAppliedFilter: defaultFilter
	property bool showEasyFilter: true

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
			source: "qrc:/icons/gear.svg"
			anchors.bottom: parent.bottom
			anchors.left: parent.left
			anchors.margins: 4
			width: 20
			height: width

			MouseArea
			{
				anchors.fill: parent
				onClicked: filterContainer.showEasyFilter = false
				hoverEnabled: true

				New.ToolTip.delay: 500
				New.ToolTip.visible: containsMouse
				New.ToolTip.text: "Click here to switch to an advanced filter-editor (using R directly)"
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

			Item
			{
				id: filterGeneratedBox
				anchors.top: parent.top
				anchors.left: parent.left
				anchors.right: parent.right

				height: filterGeneratedEdit.contentHeight

				TextArea
				{
					id: filterGeneratedEdit
					anchors.top: filterGeneratedBox.top
					anchors.left: filterGeneratedBox.left
					anchors.right: resetAllGeneratedFilters.left
					text: generatedFilter
					height: contentHeight
					readOnly: true
					textColor: "gray"
				}

				Button
				{
					id: resetAllGeneratedFilters
					anchors.right: parent.right
					anchors.verticalCenter: parent.verticalCenter
					width: dataSetModel.columnsFilteredCount > 0 ? implicitWidth : 0
					iconSource: "../images/eraser_all.png"
					visible: dataSetModel.columnsFilteredCount > 0
					anchors.margins: 1
					onClicked: dataSetModel.resetAllFilters()
				}
			}

            TextArea
            {

                id: filterEdit
                anchors.top: filterGeneratedBox.bottom
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: applyFilter.top
            }

			Button
			{
				id: easyFilterButton
				iconSource: "qrc:/icons/gear.svg"
				anchors.left: parent.left
				anchors.bottom: parent.bottom

				onClicked: filterContainer.showEasyFilter = true

				width: visible ? implicitWidth : 0
			}

			Button
			{
				id: clearFilterButton
				iconSource: "../images/eraser.png"
				anchors.left: easyFilterButton.right
				anchors.bottom: parent.bottom

				onClicked: filterWindow.resetFilter()

				width: visible ? implicitWidth : 0
				visible: filterEdit.text != defaultFilter
			}

            Button
            {
                id: applyFilter
                text: "Apply Filter"
                anchors.left: clearFilterButton.right
                anchors.right: closeFilterButton.left
                anchors.bottom: parent.bottom

                onClicked: filterWindow.applyAndSendFilter(filterEdit.text)

            }

            Button
            {
                id: closeFilterButton
                iconSource: "../images/cross.png"
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                onClicked: filterWindow.toggle()
            }
        }

        TextArea
        {
            id: filterError
            textColor: "red"
            readOnly: true
            text: filterErrorText
            Layout.minimumHeight: filterWindow.minimumHeightTextBoxes//filterError.contentHeight//Math.min(filterError.contentHeight, filterWindow.minimumHeightTextBoxes)

            states: [
                State {
                    name: "closed"
                    PropertyChanges { target: filterError; visible: false; height: 0 }
                    when: filterError.text.length == 0
                },
                State {
                    name: "opened"
                    PropertyChanges { target: filterError; visible: true; height: filterError.contentHeight} //Math.min( , filterWindow.minimumHeightTextBoxes)

                    when: filterError.text.length > 0
                }
            ]
        }


    }

}
