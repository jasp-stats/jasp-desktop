import QtQuick			2.13
import JASP.Widgets		1.0

import QtQuick.Controls	2.4

Popup
{
	id:					plotEditorPopup
	y:					(parent.height / 2) - (height / 2)
	x:					(parent.width  / 2) - (width  / 2)
	width:				parent.width  * 0.8
	height:				parent.height * 0.8
	modal:				true
	background:			Rectangle { color: jaspTheme.uiBackground }
	closePolicy:		Popup.CloseOnPressOutside | Popup.CloseOnEscape

	visible:			plotEditorModel.visible
	onVisibleChanged:	plotEditorModel.visible = visible

	Loader
	{
		visible:			plotEditorModel.visible
		sourceComponent:	visible ? plotEditorComponent : null
		anchors.fill:		parent
	}

	Component
	{
		id:		plotEditorComponent

		Rectangle
		{
			color:			jaspTheme.uiBackground
			border.color:	jaspTheme.uiBorder
			border.width:	preferencesModel.uiScale

			Flickable
			{
				anchors.fill:		parent
				anchors.margins:	jaspTheme.generalAnchorMargin
				clip:				true
				contentWidth:		width
				contentHeight:		imgOpts.height

				Item
				{
					id:			imgOpts
					width:		parent.width
					//spacing:	jaspTheme.rowSpacing
					height:		childrenRect.height

					Text
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
						width:			parent.width * 0.2

						anchors
						{
							left:			parent.left
							leftMargin:		jaspTheme.generalAnchorMargin
							top:			plotImgRect.top
							bottom:			plotImgRect.bottom
						}

						TableView
						{
							id:						yAxis
							clip:					true
							model:					plotEditorModel.yAxis
							delegate:				axisElement
							anchors.fill:			parent
							columnWidthProvider:	function(column) { return yAxis.width / 2 }
						}
					}

					Rectangle
					{
						id:					yAxisTitle
						z:					-1
						color:				jaspTheme.white
						border.color:		jaspTheme.black
						border.width:		1
						height:				jaspTheme.font.pixelSize * 2

						anchors
						{
							left:		yAxisId.left
							top:		xAxisId.top
							right:		yAxisId.right
						}

						TextInput
						{
							text:					plotEditorModel.yAxis.title
							padding:				4 * preferencesModel.uiScale
							font:					jaspTheme.font
							anchors.centerIn:		parent
							horizontalAlignment:	Text.AlignHCenter
							verticalAlignment:		Text.AlignVCenter
							onEditingFinished:		plotEditorModel.yAxis.title = text
						}
					}

					Rectangle
					{
						id:					xAxisTitle
						z:					-1
						color:				jaspTheme.white
						border.color:		jaspTheme.black
						border.width:		1
						height:				jaspTheme.font.pixelSize * 2

						anchors
						{
							left:		yAxisTitle.left
							right:		yAxisTitle.right
							bottom:		xAxisId.bottom
						}

						TextInput
						{
							text:					plotEditorModel.xAxis.title
							padding:				4 * preferencesModel.uiScale
							font:					jaspTheme.font
							anchors.centerIn:		parent
							horizontalAlignment:	Text.AlignHCenter
							verticalAlignment:		Text.AlignVCenter
							onEditingFinished:		plotEditorModel.xAxis.title = text
						}
					}

					Rectangle
					{
						id:						plotImgRect
						color:					"white"
						border.color:			"black"
						border.width:			preferencesModel.uiScale
						height:					plotImg.height	+ (2 * jaspTheme.generalAnchorMargin)
						anchors
						{
							top:				title.bottom
							left:				yAxisId.right
							right:				parent.right
							margins:			jaspTheme.generalAnchorMargin
						}

						Image
						{
							id:					plotImg
							cache:				false
							source:				plotEditorModel.imgFile
							sourceSize.width:	plotEditorModel.width
							sourceSize.height:	plotEditorModel.height
							width:				Math.max((parent.width - ( 2 * plotImgRect.border.width)), plotEditorModel.width)
							height:				(width * (plotEditorModel.height / plotEditorModel.width) + ( 2 * plotImgRect.border.width))
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
						}
					}

					Rectangle
					{
						id:				xAxisId
						color:			jaspTheme.white
						border.color:	jaspTheme.black
						border.width:	preferencesModel.uiScale
						height:			jaspTheme.font.pixelSize * 4.5

						anchors
						{
							top:		plotImgRect.bottom
							topMargin:	jaspTheme.generalAnchorMargin
							left:		plotImgRect.left
							right:		plotImgRect.right
						}

						TableView
						{
							id:				xAxis
							clip:			true
							model:			plotEditorModel.xAxis
							delegate:		axisElement
							anchors.fill:	parent
						}
					}


					Component
					{
						id:		axisElement

						Item
						{
							implicitWidth:	Math.max(textDelegate.implicitWidth, editor.implicitWidth) + ( 10 * preferencesModel.uiScale)
							implicitHeight:	jaspTheme.font.pixelSize * 2

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
									anchors.fill:	parent
									onClicked:
									{
										editor.visible = true;
										editor.forceActiveFocus();
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
							}
						}
					}

				}
			}
		}
	}
}
