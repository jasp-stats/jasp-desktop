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

	Item
	{
		anchors.fill: parent
		visible: parent.showEasyFilter

		FilterConstructor
		{
			anchors.bottom: closeEasyFilterButton.top
			anchors.right: parent.right
			anchors.left: parent.left
			anchors.top: parent.top

			id: easyFilterConstructor

			onRCodeChanged: filterContainer.rCodeChanged(rScript)

			clip: true

			function askIfChanged(closeFunc)
			{
				if(jsonChanged() || !lastCheckPassed)
				{
					easySaveDialog.closeFunc = closeFunc
					easySaveDialog.open()
				}
				else
					closeFunc()
			}

			New.Dialog
			{
				id: easySaveDialog

				x: (easyFilterConstructor.width - width) / 2
				y: (easyFilterConstructor.height - height) / 2

				modal: true
				title: "Filter Changed"
				standardButtons: New.Dialog.Save | New.Dialog.Discard | New.Dialog.Cancel
				property var closeFunc: undefined

				onAccepted:
				{
					if(easyFilterConstructor.checkAndApplyFilter())
						closeFunc()
					close()
				}

				onRejected: { close() }
				onDiscarded: { closeFunc(); close() }

				contentItem: Text
				{
					text: "There are unapplied changes to your filter, what would you like to do with them?"
					wrapMode: Text.WrapAtWordBoundaryOrAnywhere
				}
			}
		}

		FilterButton
		{
			id: rFilterButton
			icon.source: "qrc:/icons/R.png"
			anchors.left: parent.left
			anchors.bottom: parent.bottom
			anchors.top: closeEasyFilterButton.top

			onClicked: easyFilterConstructor.askIfChanged(function() { filterContainer.showEasyFilter = false } )


			width: height

			toolTip: "Switch to the advanced filter-editor (using R directly)"

		}

		FilterButton
		{
			id: applyEasyFilter
			text: easyFilterConstructor.somethingChanged ? "Apply Filter" : "Filter Applied"
			disabled: !easyFilterConstructor.somethingChanged
			anchors.left: rFilterButton.right
			anchors.right: helpEasyFilterButton.left
			anchors.bottom: parent.bottom
			anchors.top: closeEasyFilterButton.top

			onClicked: easyFilterConstructor.checkAndApplyFilter()

			toolTip: easyFilterConstructor.somethingChanged ? "Click to apply filter" : "Filter is already applied"
		}

		FilterButton
		{
			id: helpEasyFilterButton
			icon.source: "qrc:/icons/QuestionMark.png"
			anchors.right: closeEasyFilterButton.left
			anchors.bottom: parent.bottom
			anchors.top: closeEasyFilterButton.top

			onClicked: mainWindow.showHelpFromQML("other/EasyFilterConstructor");
			toolTip: "Open Documentation"
		}

		FilterButton
		{
			id: closeEasyFilterButton
			icon.source: "../images/cross.png"
			anchors.right: parent.right
			anchors.bottom: parent.bottom

			onClicked: easyFilterConstructor.askIfChanged(function() { filterWindow.toggle() } )
			toolTip: "Hide filter"
		}
	}

    SplitView
    {
		visible: !parent.showEasyFilter
        anchors.fill: parent

        orientation: Qt.Vertical

        Rectangle {
			id: filterEditRectangle
			color: "white"

            border.width: 1
			border.color: "lightGrey"
            Layout.fillHeight: true
            Layout.minimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight

			New.ScrollView
			{
				id: filterScroller
				anchors.fill: parent
				clip: true

				contentWidth: width
				contentHeight: filterGeneratedBox.height + filterEditBox.height

				Rectangle
				{
					id: filterGeneratedBox
					anchors.top: parent.top
					anchors.left: parent.left
					anchors.right: parent.right

					height: filterGeneratedEdit.contentHeight
					color: "transparent"
					border.color: "lightGray"
					border.width: 1

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

						font.family: "Courier"
						wrapMode: New.TextArea.WrapAtWordBoundaryOrAnywhere

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

						property bool changedSinceLastApply: text !== filterContainer.lastAppliedFilter

						font.family: "Courier"
						wrapMode: New.TextArea.WrapAtWordBoundaryOrAnywhere
					}
				}
			}

			function askIfChanged(closeFunc)
			{
				if(filterEdit.changedSinceLastApply)
				{
					saveDialog.closeFunc = closeFunc
					saveDialog.open()
				}
				else
					closeFunc()
			}

			New.Dialog
			{
				id: saveDialog

				x: (filterEditRectangle.width - width) / 2
				y: (filterEditRectangle.height - height) / 2

				modal: true
				title: "Filter Changed"
				standardButtons: New.Dialog.Save | New.Dialog.Discard | New.Dialog.Cancel
				property var closeFunc: undefined

				onAccepted:
				{
					filterWindow.applyAndSendFilter(filterEdit.text)
					closeFunc()
					close()
				}

				onRejected: { close() }
				onDiscarded: { closeFunc(); close() }

				contentItem: Text
				{
					text: "There are unapplied changes to your filter, what would you like to do with them?"
					wrapMode: Text.WrapAtWordBoundaryOrAnywhere
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

				font.family: "Courier"


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

		Item
		{
			id: filterButtons
			height: closeFilterButton.height
			anchors.left: parent.left
			anchors.right: parent.right

			FilterButton
			{
				id: easyFilterButton
				icon.source: "qrc:/icons/NotR.png"
				anchors.left: parent.left
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterEditRectangle.askIfChanged(function (){ filterContainer.showEasyFilter = true })

				width: visible ? height : 0

				toolTip: "Switch to the drag and drop filter-constructor"

			}

			FilterButton
			{
				id: clearFilterButton
				icon.source: "../images/eraser.png"
				anchors.left: easyFilterButton.right
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterWindow.resetFilter()

				width: visible ? implicitWidth : 0
				height: filterContainer.buttonsHeight
				visible: filterEdit.text !== defaultFilter

				toolTip: "Reset to default filter"
			}

			FilterButton
			{
				id: applyFilter

				text: filterEdit.changedSinceLastApply ? "Apply Filter" : "Filter Applied"
				disabled: !filterEdit.changedSinceLastApply
				anchors.left: clearFilterButton.right
				anchors.right: helpButton.left
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterWindow.applyAndSendFilter(filterEdit.text)

				toolTip: filterEdit.changedSinceLastApply ? "Click to apply filter" : "Filter is already applied"
			}

			FilterButton
			{
				id: helpButton
				icon.source: "qrc:/icons/QuestionMark.png"
				anchors.right: closeFilterButton.left
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: mainWindow.showHelpFromQML("other/RFilterConstructor");
				toolTip: "Open Documentation"
			}

			FilterButton
			{
				id: closeFilterButton
				icon.source: "../images/cross.png"
				anchors.right: parent.right
				anchors.bottom: parent.bottom

				onClicked: filterEditRectangle.askIfChanged(function (){ filterWindow.toggle() })
				toolTip: "Hide filter"
			}
		}
    }

}
