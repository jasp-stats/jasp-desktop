import QtQuick 2.7
import QtQuick.Controls 2.2 as New
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3
import "FilterConstructor"
import JASP.Theme 1.0

FocusScope
{
	id: filterContainer
	height: rFilterFields.desiredMinimumHeight
	Layout.minimumHeight: showEasyFilter ? easyFilterConstructor.desiredMinimumHeight : rFilterFields.desiredMinimumHeight

    visible: opened


    property bool opened: false
	property int minimumHeightTextBoxes: 50 * ppiScale
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
			anchors.bottom:	closeEasyRectangularButton.top
			anchors.right:	parent.right
			anchors.left:	parent.left
			anchors.top:	parent.top

			id: easyFilterConstructor

			onRCodeChanged: filterContainer.rCodeChanged(rScript)

			clip: true

			functionModel: ListModel
			{

				ListElement	{ type: "function";	functionName: "abs";	functionParameters: "values";			functionParamTypes: "number";						toolTip: "absolute value" }
				ListElement	{ type: "function";	functionName: "sd";		functionParameters: "values";			functionParamTypes: "number";						toolTip: "standard deviation" }
				ListElement	{ type: "function";	functionName: "var";	functionParameters: "values";			functionParamTypes: "number";						toolTip: "variance" }
				ListElement	{ type: "function";	functionName: "sum";	functionParameters: "values";			functionParamTypes: "number";						toolTip: "summation" }
				ListElement	{ type: "function";	functionName: "prod";	functionParameters: "values";			functionParamTypes: "number";						toolTip: "product of values" }

				ListElement	{ type: "function";	functionName: "min";	functionParameters: "values";			functionParamTypes: "number";							toolTip: "returns minimum of values" }
				ListElement	{ type: "function";	functionName: "max";	functionParameters: "values";			functionParamTypes: "number";								toolTip: "returns maximum of values" }
				ListElement	{ type: "function";	functionName: "mean";	functionParameters: "values";			functionParamTypes: "number";									toolTip: "mean" }
				ListElement	{ type: "function";	functionName: "round";	functionParameters: "y,n";				functionParamTypes: "number,number";								toolTip: "rounds y to n decimals" }
				ListElement	{ type: "function";	functionName: "length";	functionParameters: "y";				functionParamTypes: "string:number";									toolTip: "returns number of elements in y" }
				ListElement	{ type: "function";	functionName: "median";	functionParameters: "values";			functionParamTypes: "number";												toolTip: "median" }
				ListElement	{ type: "function";	functionName: "ifelse";	functionParameters: "test,then,else";	functionParamTypes: "boolean,boolean:string:number,boolean:string:number";		toolTip: "if-else statement" }
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

		RectangularButton
		{
			id: rRectangularButton
			iconSource: "qrc:/icons/R.png"
			anchors.left: parent.left
			anchors.bottom: parent.bottom
			anchors.top: closeEasyRectangularButton.top

			onClicked: easyFilterConstructor.askIfChanged(function() { filterContainer.showEasyFilter = false } )


			width: height

			toolTip: "Switch to the R filter"

		}

		RectangularButton
		{
			property bool showApplyNotApplied: easyFilterConstructor.somethingChanged || easyFilterConstructor.showStartupMsg

			id:				applyEasyFilter
			text:			showApplyNotApplied ? "Apply pass-through filter" : "Filter applied"
			enabled:		easyFilterConstructor.somethingChanged
			anchors.left:	rRectangularButton.right
			anchors.right:	helpEasyRectangularButton.left
			anchors.bottom: parent.bottom
			anchors.top:	closeEasyRectangularButton.top

			onClicked:		easyFilterConstructor.checkAndApplyFilter()

			toolTip:		showApplyNotApplied ? "Click to apply filter" : "Filter is already applied"
		}

		RectangularButton
		{
			id:				helpEasyRectangularButton
			iconSource:		"qrc:/icons/QuestionMark.png"
			anchors.right:	closeEasyRectangularButton.left
			anchors.bottom:	parent.bottom
			anchors.top:	closeEasyRectangularButton.top

			onClicked:		mainWindow.showHelpFromQML("other/EasyFilterConstructor");
			toolTip:		"Open Documentation"
		}

		RectangularButton
		{
			id: closeEasyRectangularButton
			iconSource: "qrc:/images/cross.png"
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
			color: Theme.white

            border.width: 1
			border.color: "lightGrey"
			//Layout.fillHeight: true
			//Layout.minimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight
			property real desiredMinimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight


			anchors.top: parent.top
			anchors.bottom: filterErrorScroll.top
			anchors.left: parent.left
			anchors.right: parent.right

			Image
			{
				id:							backgroundImage

				source:						"qrc:/icons/filterConstructorBackground.png"
				anchors.centerIn:			parent

				property real widthScale:	parent.width  / implicitWidth
				property real heightScale:	parent.height / implicitHeight
				property real ratio:		Math.min(Math.min(widthScale, heightScale), 1.0) * 0.5

				width:						implicitWidth * ratio
				height:						implicitHeight * ratio
			}

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
						font.pixelSize: baseFontSize * ppiScale
						wrapMode: New.TextArea.WrapAtWordBoundaryOrAnywhere

					}

					RectangularButton
					{
						id: resetAllGeneratedFilters
						anchors.left: parent.left
						anchors.verticalCenter: parent.verticalCenter
						width: dataSetModel.columnsFilteredCount > 0 ? height : 0
						iconSource: "qrc:/images/eraser_all.png"
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
						font.pixelSize: baseFontSize * ppiScale
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
				font.pixelSize: baseFontSize * ppiScale

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
			height: closeRectangularButton.height
			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: parent.bottom

			RectangularButton
			{
				id: easyRectangularButton
				iconSource: "qrc:/icons/NotR.png"
				anchors.left: parent.left
				anchors.bottom: parent.bottom
				anchors.top: closeRectangularButton.top

				onClicked: filterEditRectangle.askIfChanged(function (){ filterContainer.showEasyFilter = true })

				width: visible ? height : 0

				toolTip: "Switch to the drag and drop filter"

			}

			RectangularButton
			{
				id: clearRectangularButton
				iconSource: "qrc:/images/eraser.png"
				anchors.left: easyRectangularButton.right
				anchors.bottom: parent.bottom
				anchors.top: closeRectangularButton.top

				onClicked: filterWindow.resetFilter()

				width: visible ? implicitWidth : 0
				height: filterContainer.buttonsHeight
				visible: filterEdit.text !== filterModel.defaultRFilter

				toolTip: "Reset to default filter"
			}

			RectangularButton
			{
				id: applyFilter

				property bool filterIsDefault: filterEdit.text === filterModel.defaultRFilter

				text:			filterEdit.changedSinceLastApply ? "Apply pass-through filter" : filterIsDefault ? "Default filter applied" : "Filter applied"
				enabled:		filterEdit.changedSinceLastApply
				anchors.left:	clearRectangularButton.right
				anchors.right:	helpButton.left
				anchors.bottom:	parent.bottom
				anchors.top:	closeRectangularButton.top

				onClicked:		filterWindow.applyAndSendFilter(filterEdit.text)

				toolTip:		filterEdit.changedSinceLastApply ? "Click to apply filter" : filterIsDefault ? "Filter is unchanged from default" : "Filter is already applied"
			}

			RectangularButton
			{
				id: helpButton
				iconSource: "qrc:/icons/QuestionMark.png"
				anchors.right: closeRectangularButton.left
				anchors.bottom: parent.bottom
				anchors.top: closeRectangularButton.top

				onClicked: mainWindow.showHelpFromQML("other/RFilterConstructor");
				toolTip: "Open Documentation"
			}


			RectangularButton
			{
				id: closeRectangularButton
				iconSource: "qrc:/images/cross.png"
				anchors.right: parent.right
				anchors.bottom: parent.bottom

				onClicked: filterEditRectangle.askIfChanged(function (){ filterWindow.toggle() })
				toolTip: "Hide filter"
			}
		}
    }

}
