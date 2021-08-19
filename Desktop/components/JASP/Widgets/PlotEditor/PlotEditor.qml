import QtQuick				2.14
import QtQuick.Controls		2.14
import QtQuick.Controls		1.4		as OLD
import QtQuick.Layouts		1.3
import JASP.Widgets			1.0		as	JASPW
import JASP.Theme			1.0
import JASP.Controls		1.0		as	JASPC
import JASP.PlotEditor		1.0

Popup
{
	id:					plotEditorPopup
	y:					0 //(parent.height / 2) - (height / 2)
	x:					0 //(parent.width  / 2) - (width  / 2)
	width:				parent.width
	height:				parent.height
	modal:				true
	background:			Rectangle { color: jaspTheme.shadow	}
	closePolicy:		Popup.NoAutoClose

	visible:			plotEditorModel.visible
	onVisibleChanged:	plotEditorModel.visible = visible
	focus:				true

	Shortcut { onActivated: cancel();	sequence: "Escape" }

	function cancel()
	{
		plotEditorModel.cancelPlot()
		plotEditorPopup.close()
	}

	Loader
	{
		visible:			plotEditorModel.visible
		sourceComponent:	visible && !plotEditorModel.loading ? plotEditorComponent : null
		anchors.fill:		parent
		anchors.margins:	jaspTheme.generalAnchorMargin
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

			OLD.SplitView
			{
				id:				splitView
				anchors
				{
					top:			parent.top
					left:			parent.left
					right:			parent.right
					bottom:			exitButton.top
					margins:		jaspTheme.generalAnchorMargin
				}
				orientation:	Qt.Horizontal
				handleDelegate:	Rectangle
				{
					implicitWidth:	jaspTheme.splitHandleWidth * 0.8;
					color:			"transparent"

					Rectangle
					{
						anchors
						{
							fill: parent
							topMargin: axes.tabBarHeight
						}
						color: styleData.hovered || styleData.pressed ? jaspTheme.grayLighter : jaspTheme.uiBackground

						Rectangle
						{
							anchors
							{
								top: parent.top
								right: parent.right
								left: parent.left
							}
							height: 1
							color: jaspTheme.uiBorder
						}

						Rectangle
						{
							anchors
							{
								bottom: parent.bottom
								right: parent.right
								left: parent.left
							}
							height: 1
							color: jaspTheme.uiBorder
						}

						Item
						{
							id:							threeDots
							height:						width * 4
							width:						jaspTheme.splitHandleWidth * 0.3
							anchors.centerIn:			parent
							property color	kleur:		jaspTheme.grayDarker

							Rectangle
							{
								color:		threeDots.kleur
								height:		width
								radius:		width

								anchors
								{
									left:	parent.left
									right:	parent.right
									top:	parent.top
								}
							}

							Rectangle
							{
								color:		threeDots.kleur
								height:		width
								radius:		width
								anchors
								{
									left:	parent.left
									right:	parent.right
									verticalCenter:	parent.verticalCenter
								}
							}

							Rectangle
							{
								color:		threeDots.kleur
								height:		width
								radius:		width

								anchors
								{
									left:	parent.left
									right:	parent.right
									bottom:	parent.bottom
								}
							}
						}

					}
				}

				Item
				{
					id:						axes
					Layout.preferredWidth:	parent.width * .3
					Layout.minimumWidth:	parent.width * .3
					Layout.maximumWidth:	parent.width * .9

					property real	tabBarHeight:		28 * preferencesModel.uiScale
					property real	tabButtonRadius:	5 * preferencesModel.uiScale
					property real	tabButtonWidth:		100 * preferencesModel.uiScale
					property var	axeTitles:			[ qsTr("x-axis"), qsTr("y-axis") ]
					property var	axeModels:			[ plotEditorModel.xAxis, plotEditorModel.yAxis ]

					Rectangle
					{
						// Rectangle to draw the border under the tabbar
						anchors
						{
							fill:		parent
							topMargin:	axes.tabBarHeight
						}
						border.width:	1
						border.color:	jaspTheme.uiBorder
						color:			"transparent"
					}

					Flickable
					{
						id:						axesFlickable
						anchors.fill:			parent
						clip:					true

						contentHeight:			flickChild.height
						contentWidth:			flickChild.width
						flickableDirection:		Flickable.VerticalFlick

						onFlickStarted:			forceActiveFocus();

						Item
						{
							id:					flickChild
							width:				axesFlickable.width
							height:				stack.y + stack.height + jaspTheme.generalAnchorMargin

							MouseArea
							{
								id:				focusCatcher	//Because then people can click away from the axistableview or something
								onPressed:		{ forceActiveFocus(); mouse.accepted = false; }
								anchors
								{
									top:		parent.top
									left:		parent.left
									right:		parent.right
								}
								height:			Math.max(flickChild.height, axesFlickable.height)
								z:				-100
							}

							TabBar
							{
								id:				tabbar
								contentHeight:	axes.tabBarHeight + axes.tabButtonRadius
								width:			axes.axeTitles.length * axes.tabButtonWidth

								background: Rectangle { color: jaspTheme.uiBackground } // Per default the background is white

								Repeater
								{
									model: axes.axeTitles
									TabButton
									{
										height:		tabbar.height
										background: Rectangle
										{
											color:			checked ? jaspTheme.uiBackground : jaspTheme.grayLighter
											radius:			axes.tabButtonRadius
											border.width:	1
											border.color:	checked ? jaspTheme.uiBorder : jaspTheme.borderColor
										}

										contentItem: Text
										{
											// The bottom of buttons are hidden to remove their bottom line with the radius
											// So the text has to be moved higher from the horizontal middle line.
											topPadding:			-axes.tabButtonRadius * 3/4
											text:				modelData
											font:				jaspTheme.font
											color:				jaspTheme.black
											horizontalAlignment: Text.AlignHCenter
											verticalAlignment:	Text.AlignVCenter
											opacity:			checked ? 1 : .6
										}

										MouseArea
										{
											anchors.fill	: parent
											cursorShape		: checked ? Qt.ArrowCursor : Qt.PointingHandCursor
											acceptedButtons	: Qt.NoButton
										}
									}
								}
							}

							Rectangle
							{
								// This hides the bottom border of the buttons (with their radius)
								id		: roundingHider
								width	: parent.width
								height	: axes.tabButtonRadius + 1
								anchors
								{
									left:		parent.left
									right:		tabbar.right
									top:		parent.top
									topMargin:	axes.tabBarHeight
								}
								color: jaspTheme.uiBackground

								Rectangle
								{
									// The Tabbar removes the left border. Redraw it.
									anchors.left:	parent.left
									anchors.top:	parent.top
									anchors.bottom: parent.bottom
									width:			1
									color:			jaspTheme.uiBorder
								}
							}

							Rectangle
							{
								// Redraw a line below the unchecked tab
								anchors
								{
									top:			roundingHider.top
									left:			parent.left
									leftMargin:		tabbar.currentIndex === 0 ? axes.tabButtonWidth - 1 : 0
									right:			tabbar.right
									rightMargin:	tabbar.currentIndex === 0 ? 0 : axes.tabButtonWidth  - 1
								}
								height:	1
								color:	jaspTheme.uiBorder
							}

							StackLayout
							{
								id: stack
								anchors
								{
									top			: tabbar.bottom
									left		: parent.left
									right		: parent.right
									margins		: jaspTheme.generalAnchorMargin
								}
								currentIndex: tabbar.currentIndex

								Repeater
								{
									model: axes.axeModels
									PlotEditingAxis
									{
										axisModel:		modelData
										width:			flickChild.width
									}
								}
							}
						}
					}

					/* Redo/Undo buttons: let's do that later.
					JASPW.MenuButton
					{
						id:					redoButton
						iconSource:			jaspTheme.iconPath + "/redo.svg"
						enabled:			plotEditorModel.redoEnabled
						toolTip:			qsTr("Redo last change")
						radius:				height
						width:				height
						opacity:			enabled ? 1 : 0.1
						anchors
						{
							top:			axesFlickable.top
							right:			axesFlickable.right
							// same as in AnalysisFormExpandser.qml
							topMargin:		-4 * preferencesModel.uiScale
							bottomMargin:	6 * preferencesModel.uiScale
						}
						onClicked:			plotEditorModel.redoSomething()
					}

					JASPW.MenuButton
					{
						id:					undoButton
						iconSource:			jaspTheme.iconPath + "/undo.svg"
						enabled:			plotEditorModel.undoEnabled
						toolTip:			qsTr("Undo last change")
						radius:				height
						width:				height
						opacity:			enabled ? 1 : 0.2
						anchors
						{
							top:			axesFlickable.top
							right:			redoButton.left
							topMargin:		redoButton.anchors.topMargin
							bottomMargin:	redoButton.anchors.bottomMargin

						}
						onClicked:			plotEditorModel.undoSomething()
					} */

					JASPC.JASPScrollBar
					{
						id:				axesScrollbar
						flickable:		axesFlickable
						vertical:		true
					}
				}

				Item
				{
					id:						plotImgParent

					Rectangle
					{
						id:					plotImgRect
						color:				jaspTheme.themeName === "darkTheme" ? "black" : jaspTheme.white
						border.color:		jaspTheme.uiBorder
						border.width:		1
						anchors.fill:		parent
						anchors.topMargin: 	axes.tabBarHeight

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
							x:					plotImgRect.border.width + (parent.width  - (width  + plotImgRect.border.width * 2)) * 0.5
							y:					plotImgRect.border.width + (parent.height - (height + plotImgRect.border.width * 2)) * 0.5
							mipmap:				true

							MouseArea
							{
								// To Do Vincent Pedata: For now we will just work with a single MouseArea that handles the clicks. It will just show a pointing hand all the time
								// Later we can make it so that it only shows the pointing hand when it overlaps a clickable element (which informs the user and makes it feel a bit more interactive)
								id:				elementPicker
								anchors.fill:	parent
								cursorShape:	Qt.ArrowCursor
								onClicked:
								{
									forceActiveFocus();

									/*
									var x = mouse.x; //real and relative to mouseArea
									var y = mouse.y; //real

									// To Do Vincent Pedata: Make sure that the x and y here are scaled properly for your coordinates
									var pickedElement = plotEditorModel.clickHitsElement(x, y);

									if(pickedElement !== "")
										message("Element " + pickedElement + " was picked!"); // To Do Vincent Pedata: This should obviously be something more than just a msg :p
									*/
								}
							}

							JASPW.ImageInverter
							{
								src:			plotImg
							}
						}
					}
				}
			}

			JASPW.RoundedButton
			{
				id:					exitButton
				anchors
				{
					right:			parent.right
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}
				text:				qsTr("OK")
				width:				cancelButton.width
				color:				jaspTheme.blueLighter
				on_PressedChanged:	plotEditorPopup.close()
			}

			JASPW.RoundedButton
			{
				id:					cancelButton
				anchors
				{
					right:			exitButton.left
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}
				text:				qsTr("Cancel")
				buttonPadding:		20 * preferencesModel.uiScale
				on_PressedChanged:	cancel()
			}

			JASPW.RoundedButton
			{
				id:					resetDefaultButton
				anchors
				{
					left:			parent.left
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}
				text:				qsTr("Reset defaults")
				onClicked:			plotEditorModel.resetDefaults()
			}

			JASPW.RoundedButton
			{
				id:					saveButton
				anchors
				{
					left:			resetDefaultButton.right
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}
				text:				qsTr("Save image as")
				onClicked:			plotEditorModel.savePlot()
			}

		}
	}
}
