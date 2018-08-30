import QtQuick 2.7
import QtQuick.Controls 2.2 as New
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3
import "FilterConstructor"

FocusScope
{
	id: filterContainer
	height: rFilterFields.desiredMinimumHeight
	Layout.minimumHeight: showEasyFilter ? easyFilterConstructor.desiredMinimumHeight : rFilterFields.desiredMinimumHeight

    visible: opened


    property bool opened: false
	property int minimumHeightTextBoxes: 50
	property bool showEasyFilter: true

	onShowEasyFilterChanged: if(!showEasyFilter) absorbModelRFilter()
	onVisibleChanged: if(!visible) filterWindow.close()

    function toggle()
    {
        opened = !opened
		absorbModelRFilter()

		if(opened)
			height = parent.height / 2
    }

	function open()
	{
		if(!opened)
			toggle();
	}

	function close()
	{
		if(opened)
			toggle();
	}


    function applyAndSendFilter(newFilter)
    {
		filterModel.rFilter = newFilter //Triggers send in FilterModel
		absorbModelRFilter()
    }

	function resetFilter()
	{
		filterModel.resetRFilter()
		absorbModelRFilter()
	}

	function absorbModelRFilter()
	{
		filterEdit.text = filterModel.rFilter
	}

	signal rCodeChanged(string rScript)

	Item
	{
		anchors.fill:	parent
		visible:		parent.showEasyFilter

		FilterConstructor
		{
			anchors.bottom:	closeEasyFilterButton.top
			anchors.right:	parent.right
			anchors.left:	parent.left
			anchors.top:	parent.top

			id: easyFilterConstructor

			onRCodeChanged: filterContainer.rCodeChanged(rScript)

			clip: true

			functionModel: ListModel
			{

				ListElement	{ type: "function";	functionName: "abs";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "absolute value" }
				ListElement	{ type: "function";	functionName: "sd";		functionParameters: "values";	functionParamTypes: "number";			toolTip: "standard deviation" }
				ListElement	{ type: "function";	functionName: "var";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "variance" }
				ListElement	{ type: "function";	functionName: "sum";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "summation" }
				ListElement	{ type: "function";	functionName: "prod";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "product of values" }

				ListElement	{ type: "function";	functionName: "min";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "returns minimum of values" }
				ListElement	{ type: "function";	functionName: "max";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "returns maximum of values" }
				ListElement	{ type: "function";	functionName: "mean";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "mean" }
				ListElement	{ type: "function";	functionName: "round";	functionParameters: "y,n";		functionParamTypes: "number,number";	toolTip: "rounds y to n decimals" }
				ListElement	{ type: "function";	functionName: "length";	functionParameters: "y";		functionParamTypes: "any";				toolTip: "returns number of elements in y" }
				ListElement	{ type: "function";	functionName: "median";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "median" }
			}

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
                property var closeFunc: undefined

                footer: New.DialogButtonBox
                {
                    New.Button
                    {
                        text: qsTr("Save")
                        onClicked:
                        {
                            if(easyFilterConstructor.checkAndApplyFilter())
                                easySaveDialog.closeFunc();
                            easySaveDialog.close();
                        }
                    }

                    New.Button {
                        text: qsTr("Cancel")

                        onClicked:
                        {
                            easySaveDialog.close();
                        }

                    }
                    New.Button {
                        text: qsTr("Discard")

                        onClicked:
                        {
                            easySaveDialog.closeFunc();
                            easySaveDialog.close();
                        }

                    }
                }

				contentItem: Text
				{
					text: "There are unapplied changes to your filter; what would you like to do?"
					wrapMode: Text.WrapAtWordBoundaryOrAnywhere
				}
			}
		}

		FilterButton
		{
			id: rFilterButton
			iconSource: "qrc:/icons/R.png"
			anchors.left: parent.left
			anchors.bottom: parent.bottom
			anchors.top: closeEasyFilterButton.top

			onClicked: easyFilterConstructor.askIfChanged(function() { filterContainer.showEasyFilter = false } )


			width: height

			toolTip: "Switch to the R filter"

		}

		FilterButton
		{
			id: applyEasyFilter
			property bool showApplyNotApplied: easyFilterConstructor.somethingChanged || easyFilterConstructor.showStartupMsg
			text: showApplyNotApplied ? "Apply pass-through filter" : "Filter applied"
			disabled: !easyFilterConstructor.somethingChanged
			anchors.left: rFilterButton.right
			anchors.right: helpEasyFilterButton.left
			anchors.bottom: parent.bottom
			anchors.top: closeEasyFilterButton.top

			onClicked: easyFilterConstructor.checkAndApplyFilter()

			toolTip: showApplyNotApplied ? "Click to apply filter" : "Filter is already applied"
		}

		FilterButton
		{
			id: helpEasyFilterButton
			iconSource: "qrc:/icons/QuestionMark.png"
			anchors.right: closeEasyFilterButton.left
			anchors.bottom: parent.bottom
			anchors.top: closeEasyFilterButton.top

			onClicked: mainWindow.showHelpFromQML("other/EasyFilterConstructor");
			toolTip: "Open Documentation"
		}

		FilterButton
		{
			id: closeEasyFilterButton
			iconSource: "../images/cross.png"
			anchors.right: parent.right
			anchors.bottom: parent.bottom

			onClicked: easyFilterConstructor.askIfChanged(function() { filterWindow.toggle() } )
			toolTip: "Hide filter"
		}
	}

	Item
    {
		id: rFilterFields
		visible: !parent.showEasyFilter
        anchors.fill: parent
		property real desiredMinimumHeight: filterButtons.height + (filterErrorScroll.visible ? filterErrorScroll.height : 0 ) + filterEditRectangle.desiredMinimumHeight

		//orientation: Qt.Vertical

        Rectangle {
			id: filterEditRectangle
			color: "white"

            border.width: 1
			border.color: "lightGrey"
			//Layout.fillHeight: true
			//Layout.minimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight
			property real desiredMinimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight


			anchors.top: parent.top
			anchors.bottom: filterErrorScroll.top
			anchors.left: parent.left
			anchors.right: parent.right

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
						text: filterModel.generatedFilter +"\n"
						height: contentHeight
						readOnly: true
						color: "gray"
						selectByMouse: true
						onActiveFocusChanged: if(!activeFocus) deselect()

						font.family: "Courier"
						wrapMode: New.TextArea.WrapAtWordBoundaryOrAnywhere

					}

					FilterButton
					{
						id: resetAllGeneratedFilters
						anchors.left: parent.left
						anchors.verticalCenter: parent.verticalCenter
						width: dataSetModel.columnsFilteredCount > 0 ? height : 0
						iconSource: "../images/eraser_all.png"
						visible: dataSetModel.columnsFilteredCount > 0
						anchors.margins: 1
						onClicked: dataSetModel.resetAllFilters()
						height: filterGeneratedBox.height
						toolTip: "Reset all checkmarks on all labels"

						//background: Rectangle {	color: "transparent" }
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

						property bool changedSinceLastApply: text !== filterModel.rFilter

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
                property var closeFunc: undefined

                footer: New.DialogButtonBox
                {
                    New.Button
                    {
                        text: qsTr("Save")
                        onClicked:
                        {
                            filterWindow.applyAndSendFilter(filterEdit.text)
                            saveDialog.closeFunc()
                            saveDialog.close()
                        }
                    }

                    New.Button {
                        text: qsTr("Cancel")

                        onClicked:
                        {
                            saveDialog.close()
                        }

                    }
                    New.Button {
                        text: qsTr("Discard")

                        onClicked:
                        {
                            saveDialog.closeFunc();
                            saveDialog.close();
                        }

                    }
                }

				contentItem: Text
				{
					text: "There are unapplied changes to your filter; what would you like to do?"
					wrapMode: Text.WrapAtWordBoundaryOrAnywhere
				}
			}
        }

		New.ScrollView
		{
			id: filterErrorScroll
			height: filterWindow.minimumHeightTextBoxes//filterError.contentHeight//Math.min(filterError.contentHeight, filterWindow.minimumHeightTextBoxes)

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: filterButtons.top

			New.TextArea
			{
				id: filterError
				color: "red"
				readOnly: true
				text: filterModel.filterErrorMsg + "\n"

				selectByMouse: true
				onActiveFocusChanged: if(!activeFocus) deselect()

				font.family: "Courier"


				states: [
					State {
						name: "closed"
						PropertyChanges { target: filterErrorScroll; visible: false; height: 0 }
						when: filterModel.filterErrorMsg.length === 0
					},
					State {
						name: "opened"
						PropertyChanges { target: filterErrorScroll; visible: true; height: filterError.contentHeight} //Math.min( , filterWindow.minimumHeightTextBoxes)

						when: filterModel.filterErrorMsg.length > 0
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
			anchors.bottom: parent.bottom

			FilterButton
			{
				id: easyFilterButton
				iconSource: "qrc:/icons/NotR.png"
				anchors.left: parent.left
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterEditRectangle.askIfChanged(function (){ filterContainer.showEasyFilter = true })

				width: visible ? height : 0

				toolTip: "Switch to the drag and drop filter"

			}

			FilterButton
			{
				id: clearFilterButton
				iconSource: "../images/eraser.png"
				anchors.left: easyFilterButton.right
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterWindow.resetFilter()

				width: visible ? implicitWidth : 0
				height: filterContainer.buttonsHeight
				visible: filterEdit.text !== filterModel.defaultRFilter

				toolTip: "Reset to default filter"
			}

			FilterButton
			{
				id: applyFilter

				property bool filterIsDefault: filterEdit.text === filterModel.defaultRFilter

				text: filterEdit.changedSinceLastApply ? "Apply pass-through filter" : filterIsDefault ? "Default filter applied" : "Filter applied"
				disabled: !filterEdit.changedSinceLastApply
				anchors.left: clearFilterButton.right
				anchors.right: helpButton.left
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: filterWindow.applyAndSendFilter(filterEdit.text)

				toolTip: filterEdit.changedSinceLastApply ? "Click to apply filter" : filterIsDefault ? "Filter is unchanged from default" : "Filter is already applied"
			}

			FilterButton
			{
				id: helpButton
				iconSource: "qrc:/icons/QuestionMark.png"
				anchors.right: closeFilterButton.left
				anchors.bottom: parent.bottom
				anchors.top: closeFilterButton.top

				onClicked: mainWindow.showHelpFromQML("other/RFilterConstructor");
				toolTip: "Open Documentation"
			}


			FilterButton
			{
				id: closeFilterButton
				iconSource: "../images/cross.png"
				anchors.right: parent.right
				anchors.bottom: parent.bottom

				onClicked: filterEditRectangle.askIfChanged(function (){ filterWindow.toggle() })
				toolTip: "Hide filter"
			}
		}
    }

}
