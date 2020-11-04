import QtQuick				2.14
import QtQuick.Controls		2.14
import Qt.labs.qmlmodels	1.0
import QtQuick.Layouts		1.3
import JASP.Widgets			1.0		as	JASPW
import JASP.Theme			1.0
import JASP.Controls		1.0		as	JASPC

Popup
{
	id:					plotEditorPopup
	y:					0 //(parent.height / 2) - (height / 2)
	x:					0 //(parent.width  / 2) - (width  / 2)
	width:				parent.width
	height:				parent.height
	modal:				true
	background:			Rectangle { color: jaspTheme.shadow	}
	closePolicy:		Popup.CloseOnPressOutside | Popup.CloseOnEscape

	visible:			plotEditorModel.visible
	onVisibleChanged:	plotEditorModel.visible = visible

	Loader
	{
		visible:			plotEditorModel.visible
		sourceComponent:	visible ? plotEditorComponent : null
		anchors.fill:		parent
		anchors.margins:	Math.min(parent.width, parent.height) * 0.1
	}

	Component
	{
		id:		plotEditorComponent

		Rectangle
		{
			color:			jaspTheme.uiBackground
			border.color:	jaspTheme.uiBorder
			border.width:	preferencesModel.uiScale

			JASPC.Text
			{
				id:							title
				font:						jaspTheme.fontLabel
				text:						plotEditorModel.title
				anchors.horizontalCenter:	parent.horizontalCenter
				y:							jaspTheme.generalAnchorMargin
			}

			Rectangle
			{
				id:				yAxisId
				color:			jaspTheme.white
				border.color:	jaspTheme.black
				border.width:	preferencesModel.uiScale
				width:			parent.width * 0.25

				anchors
				{
					left:			parent.left
					leftMargin:		jaspTheme.generalAnchorMargin
					top:			plotImgParent.top
					bottom:			plotImgParent.bottom
				}

				JASPC.Group
				{

					id : xAxis
					width: parent.width
					anchors
					{
						left:			parent.left
						leftMargin:		jaspTheme.generalAnchorMargin
						top:			parent.top
					}


					title: qsTr("x-axis")
					JASPC.TextField
					{
						id:					xaxisTitle
						name:				"labelXAxis";
						label:				qsTr("Name");
						fieldWidth:			200
						value:				plotEditorModel.xAxis.title
						onEditingFinished:	plotEditorModel.xAxis.title = value
					}

					JASPC.DropDown
					{
						// TODO: make this work on the R side (if I've got time left)
						visible: false
						name: "xAxisTitleType"
						label: qsTr("Text Type")
						values:
						[
							{ label: qsTr("Plain Text"),		value: "plainText"		},
							{ label: qsTr("R Expression"),		value: "rExpression"	},
							{ label: qsTr("LateX"),				value: "Latex"			}
						]
						indexDefaultValue: {
							var idx = ["plainText", "rExpression", "Latex"].findIndex(e => e === plotEditorModel.xAxis.titleType);
							if (idx < 0)
							{
								console.log("Someone made a typo! titleType: '", plotEditorModel.xAxis.titleType, "' could not be found!")
								idx = 0
							}
							return idx
						}
						onCurrentIndexChanged: plotEditorModel.xAxis.titleType = currentValue
					}

					JASPC.RadioButtonGroup
					{
						id:		xAxisBreaksRadioButton
						name:	"axisBreaks";
						title:	qsTr("Breaks")

						JASPC.RadioButton { id:xAxisBreaksRange;	value: "range";		label:	qsTr("Specify range");		checked: plotEditorModel.xAxis.breaksType === "range"	}
						JASPC.RadioButton {							value: "manual";	label:	qsTr("Manually");			checked: plotEditorModel.xAxis.breaksType === "manual"	}

						onValueChanged: plotEditorModel.xAxis.breaksType = xAxisBreaksRadioButton.value

					}

					JASPC.DoubleField	{	visible: xAxisBreaksRange.checked;	id: xAxisBreaksRangeFrom;	label: qsTr("from");	name: "from";		max: plotEditorModel.xAxis.to;		defaultValue: plotEditorModel.xAxis.from;	onValueChanged: plotEditorModel.xAxis.from	= value;	Layout.leftMargin: 2 * jaspTheme.indentationLength; negativeValues: true	}
					JASPC.DoubleField	{	visible: xAxisBreaksRange.checked;	id: xAxisBreaksRangeTo;		label: qsTr("to");		name: "to";			min: plotEditorModel.xAxis.from;	defaultValue: plotEditorModel.xAxis.to;		onValueChanged: plotEditorModel.xAxis.to	= value;	Layout.leftMargin: 2 * jaspTheme.indentationLength; negativeValues: true	}
					JASPC.DoubleField	{	visible: xAxisBreaksRange.checked;	id: xAxisBreaksRangeSteps;	label: qsTr("steps");	name: "steps";		min: 0;								defaultValue: plotEditorModel.xAxis.steps;	onValueChanged: plotEditorModel.xAxis.steps	= value;	Layout.leftMargin: 2 * jaspTheme.indentationLength;							}

					// @Joris an attempt to reuse JASPDataView
					FocusScope
					{
						id: __myRoot
						visible:					!xAxisBreaksRange.checked
						Layout.alignment:			Qt.AlignLeft
						Layout.preferredHeight:		125
						Layout.fillWidth: 			true
						Layout.preferredWidth:		parent.width
						Layout.margins:				jaspTheme.generalAnchorMargin

						Rectangle
						{
							color:			jaspTheme.white
							anchors.fill:	parent
							z:				-1
							border.width:	1
							border.color:	jaspTheme.uiBorder

							JASPW.JASPDataView
							{
								focus:						__myRoot.focus
								id:							dataTableView
								anchors.top:				parent.top
								anchors.left:				parent.left
								anchors.right:				parent.right
								anchors.bottom:				parent.bottom
								Layout.preferredWidth:		parent.width
								Layout.minimumWidth:		parent.width
								rowNumberWidth:				100

								model:						plotEditorModel.xAxis
//										itemDelegate:				axisElement
								itemDelegate:
									Text
									{
										width: 100
										height: 50
										text:				itemText
										color:				jaspTheme.textEnabled
										verticalAlignment:	Text.AlignVCenter
									}

								rowNumberDelegate:	Rectangle
								{
									width:			dataTableView.rowNumberWidth
									height:			50
									border.width:	1
									color:			jaspTheme.green
									Text {			text: headerText;	anchors.centerIn: parent; font: dataTableView.font	}
								}
//										columnHeaderDelegate:	Rectangle	{implicitHeight: 0;	implicitWidth: 0; color: jaspTheme.white}

							}
						}
					}

					// @Joris This is what I had, the width of the cells that contain the label/ breaks text is a bit too big imo.
					Rectangle
					{
						visible:					!xAxisBreaksRange.checked
						Layout.alignment:			Qt.AlignLeft
						Layout.preferredHeight:		125
						Layout.fillWidth: 			true
						Layout.preferredWidth:		parent.width
						Layout.margins:				jaspTheme.generalAnchorMargin

						Column
						{
							id: axisBreaksRowHeader
							clip: true
							spacing: 1

							Rectangle
							{
								implicitWidth: 100
								implicitHeight: 50
								border.width: 1
								color: jaspTheme.analysisBackgroundColor
								Text {	text: qsTr("break");	anchors.centerIn: parent	}
							}
							Rectangle
							{
								implicitWidth: 100
								implicitHeight: 50
								border.width: 1
								color: jaspTheme.analysisBackgroundColor
								Text {	text: qsTr("label");	anchors.centerIn: parent	}
							}
						}

						TableView
						{
							Layout.preferredHeight: 100
							Layout.preferredWidth: 50

							anchors
							{
								top:	parent.top
								right:	parent.right
								left:	axisBreaksRowHeader.right
								bottom: parent.bottom
								leftMargin: 1
							}

							id:				xAxisTable
							clip:			true
							model:			plotEditorModel.xAxis
							delegate:		axisElement


						}
					}

					JASPC.RadioButtonGroup
					{
						id:		xAxisLimitsRadioButton
						name:	"axisLimits";
						title:	qsTr("Limits")

						JASPC.RadioButton	{								value: "data";		label:	qsTr("Based on data");		checked: plotEditorModel.xAxis.limitsType === "data"	}
						JASPC.RadioButton	{								value: "breaks";	label:	qsTr("Based on breaks");	checked: plotEditorModel.xAxis.limitsType === "breaks"	}
						JASPC.RadioButton	{	id: axisLimitsManual;		value: "manual";	label:	qsTr("Set manually");		checked: plotEditorModel.xAxis.limitsType === "manual"	}

						onValueChanged: plotEditorModel.xAxis.limitsType = xAxisLimitsRadioButton.value
					}

					JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: xAxisLimitsLower;	label: qsTr("lower limit");	negativeValues: true;	name: "lowerLimit";		max: plotEditorModel.xAxis.limitUpper;		defaultValue: plotEditorModel.xAxis.limitLower;	onValueChanged: plotEditorModel.xAxis.limitLower = value; Layout.leftMargin: 2 * jaspTheme.indentationLength	}
					JASPC.DoubleField	{	visible: axisLimitsManual.checked;	id: xAxisLimitsUpper;	label: qsTr("upper limit");	negativeValues: true;	name: "upperLimit";		min: plotEditorModel.xAxis.limitLower;		defaultValue: plotEditorModel.xAxis.limitUpper;	onValueChanged: plotEditorModel.xAxis.limitUpper = value; Layout.leftMargin: 2 * jaspTheme.indentationLength	}

				}

				JASPC.Group
				{
					title: qsTr("y-axis")

					id : yAxis
					anchors
					{
						left:			parent.left
						leftMargin:		jaspTheme.generalAnchorMargin
						top:			xAxis.bottom
					}


					JASPC.TextField
					{
						name:				"labelYAxis"
						label:				qsTr("y-axis title")
						fieldWidth:			200
						value:				plotEditorModel.yAxis.title
						onEditingFinished:	plotEditorModel.yAxis.title = value
					}
				}
			}

			Item
			{
				id:						plotImgParent

				anchors
				{
					top:				title.bottom
					left:				yAxisId.right
					right:				parent.right
					bottom:				parent.bottom
					margins:			jaspTheme.generalAnchorMargin
				}

				Rectangle
				{
					id:						plotImgRect
					color:					"white"
					border.color:			jaspTheme.uiBorder
					border.width:			preferencesModel.uiScale
					z:						-1
					anchors.fill:			plotImg
					anchors.margins:		-plotImgRect.border.width
				}

				Image
				{
					id:					plotImg
					cache:				false
					source:				plotEditorModel.imgFile
					// same as in writeImage.R
					sourceSize.width:	plotEditorModel.width  * plotEditorModel.ppi / 96
					sourceSize.height:	plotEditorModel.height * plotEditorModel.ppi / 96

					property real widthScale:  (parent.width  - 2 * plotImgRect.border.width) / sourceSize.width
					property real heightScale: (parent.height - 2 * plotImgRect.border.width) / sourceSize.height
					property real properScale: Math.min(widthScale, heightScale)

					width:				sourceSize.width  * properScale  //Math.max(parent.width, plotEditorModel.width) - ( 2 * plotImgRect.border.width)
					height:				sourceSize.height * properScale  //width * (plotEditorModel.height / plotEditorModel.width)
					x:					plotImgRect.border.width
					y:					plotImgRect.border.width
					mipmap:				true

					MouseArea
					{
						// To Do Vincent Pedata: For now we will just work with a single MouseArea that handles the clicks. It will just show a pointing hand all the time
						// Later we can make it so that it only shows the pointing hand when it overlaps a clickable element (which informs the user and makes it feel a bit more interactive)
						id:				elementPicker
						anchors.fill:	parent
						cursorShape:	Qt.PointingHandCursor
						onClicked:
						{
							var x = mouse.x; //real and relative to mouseArea
							var y = mouse.y; //real

							// To Do Vincent Pedata: Make sure that the x and y here are scaled properly for your coordinates
							var pickedElement = plotEditorModel.clickHitsElement(x, y);

							if(pickedElement !== "")
								message("Element " + pickedElement + " was picked!"); // To Do Vincent Pedata: This should obviously be something more than just a msg :p
						}
					}

					JASPW.ImageInverter
					{
						src:			plotImg
					}
				}
			}

			Component
			{
				id:		axisElement

				Rectangle
				{
					id:						tableRect
					color:					jaspTheme.white
					border.color:			mouseArea1.containsMouse ? jaspTheme.black : jaspTheme.blue
					border.width:			preferencesModel.uiScale

					Item
					{
						implicitWidth:	Math.max(textDelegate.implicitWidth, editor.implicitWidth) + ( 5 * preferencesModel.uiScale)
						implicitHeight:	jaspTheme.font.pixelSize// * 2
						anchors.centerIn: parent

						Menu
						{
							id: contextMenu
							MenuItem { text: qsTr("Insert break left");		onTriggered: model.insertLeft	= 2}
							MenuItem { text: qsTr("Insert break right");	onTriggered: model.insertRight	= 2}
							MenuItem { text: qsTr("Delete break");			onTriggered: model.deleteBreak	= 2}
						}

						Text
						{
							id:						textDelegate
							text:					display
							padding:				4 * preferencesModel.uiScale
							font:					jaspTheme.font
							visible:				!editor.visible
							horizontalAlignment:	Text.AlignHCenter
							verticalAlignment:		Text.AlignVCenter
							anchors.centerIn:		parent


							MouseArea
							{
								id: mouseArea1
								hoverEnabled: true
								anchors.fill:		parent
								acceptedButtons:	Qt.LeftButton | Qt.RightButton
								onClicked:
								{
									if (mouse.button === Qt.RightButton)
									{
										console.log("Right")
										contextMenu.popup()
									}
									else
									{
										console.log("Left")
										editor.visible = true;
										editor.forceActiveFocus();
									}
								}
								onPressAndHold:
								{
									if (mouse.source === Qt.MouseEventNotSynthesized)
										contextMenu.popup()
								}
							}
						}

						TextInput
						{
							id:						editor
							z:						2
							text:					display
							padding:				4 * preferencesModel.uiScale
							font:					jaspTheme.font
							anchors.centerIn:		parent
							horizontalAlignment:	Text.AlignHCenter
							verticalAlignment:		Text.AlignVCenter
							visible:				false

							onEditingFinished:
							{
								model.display	= editor.text
								focus			= false;
							}

							onActiveFocusChanged:	if(!activeFocus)
													{
														visible = false;
														text	= Qt.binding(function(){ return display; });
													}
							Rectangle
							{
								z:					-1
								color:				jaspTheme.white
								border.color:		jaspTheme.black
								border.width:		1
								anchors.fill:		parent
							}

							MouseArea
							{
								anchors.fill:		parent
								acceptedButtons:	Qt.LeftButton | Qt.RightButton
								onClicked:
								{
									if (mouse.button === Qt.RightButton)
									{
										console.log("Right")
										contextMenu.popup()
									}
									else
									{
										console.log("Left")
										editor.visible = true;
										editor.forceActiveFocus();
									}
								}
								onPressAndHold:
								{
									if (mouse.source === Qt.MouseEventNotSynthesized)
										contextMenu.popup()
								}
							}
						}
					}
				}
			}
		}
	}
}
