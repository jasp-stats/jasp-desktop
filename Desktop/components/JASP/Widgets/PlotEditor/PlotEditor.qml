import QtQuick				2.14
import QtQuick.Controls		2.14
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
	closePolicy:		Popup.CloseOnPressOutside | Popup.CloseOnEscape

	visible:			plotEditorModel.visible
	onVisibleChanged:	plotEditorModel.visible = visible

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

			MouseArea
			{
				id:				focusCatcher	//Because then people can click away from the axistableview or something
				onPressed:		{ forceActiveFocus(); mouse.accepted = false; }
				anchors.fill:	parent
				z:				100
				
			}

			JASPC.Text
			{
				id:							title
				font:						jaspTheme.fontLabel
				text:						plotEditorModel.title
				anchors.horizontalCenter:	parent.horizontalCenter
				y:							jaspTheme.generalAnchorMargin
			}

			Item
			{
				id:				axes
				width:			500 * jaspTheme.uiScale
				anchors
				{
					top:		title.bottom
					left:		parent.left
					bottom:		buttonSeparator.top
					margins:	jaspTheme.generalAnchorMargin
				}

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
				}

				JASPC.JASPScrollBar
				{
					id:				axesScrollbar
					flickable:		axesFlickable
					vertical:		true
				}



				JASPC.CheckBox
				{
					id:					advanced
					label:				qsTr("Advanced settings")
					checked:			false
					anchors
					{
						right:			parent.right
						bottom:			parent.bottom
						margins:		jaspTheme.generalAnchorMargin
					}
					onCheckedChanged:	plotEditorModel.advanced = this.checked

				}
			}
			
			Rectangle
			{
				id:				buttonSeparator
				height:			1
				color:			jaspTheme.uiBorder
				anchors
				{
					bottom:			exitButton.top
					bottomMargin:	jaspTheme.generalAnchorMargin
					left:			parent.left
					right:			parent.right
				}
			}

			JASPW.RectangularButton
			{
				id:					exitButton
				anchors
				{
					left:			parent.left
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}
				text:				qsTr("Finish")
				on_PressedChanged:	plotEditorPopup.close()
			}

			JASPW.RectangularButton
			{
				id:					resetButton
				anchors
				{
					left:			exitButton.right
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}
				text:				qsTr("Original plot")
				on_PressedChanged:	plotEditorModel.resetPlot()
			}

			JASPW.RectangularButton
			{
				id:					saveButton
				anchors
				{
					left:			resetButton.right
					bottom:			parent.bottom
					margins:		jaspTheme.generalAnchorMargin
				}
				text:				qsTr("Save plot as")
				onClicked:			plotEditorModel.savePlot()
			}

			Item
			{
				id:					plotImgParent

				anchors
				{
					top:			axes.top
					left:			axes.right
					right:			parent.right
					bottom:			axes.bottom
					margins:		jaspTheme.generalAnchorMargin
				}

				Rectangle
				{
					id:					plotImgRect
					color:				"white"
					border.color:		jaspTheme.uiBorder
					border.width:		preferencesModel.uiScale
					z:					-1
					anchors.fill:		plotImg
					anchors.margins:	-plotImgRect.border.width
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
					x:					plotImgRect.border.width + (parent.width  - (width  + plotImgRect.border.width * 2)) * 0.5
					y:					plotImgRect.border.width + (parent.height - (height + plotImgRect.border.width * 2)) * 0.5
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
							forceActiveFocus();

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
		}
	}
}
