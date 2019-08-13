import QtQuick			2.13
import JASP.Widgets		1.0
import JASP.Theme		1.0
import QtQuick.Controls	2.4

Popup
{
	id:					plotEditorPopup
	y:					(parent.height / 2) - (height / 2)
	x:					(parent.width  / 2) - (width  / 2)
	width:				parent.width  * 0.8
	height:				parent.height * 0.8
	modal:				true
	background:			Rectangle { color: Theme.uiBackground }
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
			color:			Theme.uiBackground
			border.color:	Theme.uiBorder
			border.width:	preferencesModel.uiScale

			Flickable
			{
				anchors.fill:		parent
				anchors.margins:	Theme.generalAnchorMargin
				clip:				true
				contentWidth:		width
				contentHeight:		imgOpts.height

				Item
				{
					id:			imgOpts
					width:		parent.width
					//spacing:	Theme.rowSpacing
					height:		childrenRect.height

					Text
					{
						id:							title
						font:						Theme.fontLabel
						text:						plotEditorModel.title
						anchors.horizontalCenter:	parent.horizontalCenter
						y:							Theme.generalAnchorMargin
					}

					Rectangle
					{
						id:				yAxisId
						color:			Theme.white
						border.color:	Theme.black
						border.width:	preferencesModel.uiScale
						width:			parent.width * 0.2

						anchors
						{
							left:			parent.left
							leftMargin:		Theme.generalAnchorMargin
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
						color:				Theme.white
						border.color:		Theme.black
						border.width:		1
						height:				Theme.font.pixelSize * 2

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
							font:					Theme.font
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
						color:				Theme.white
						border.color:		Theme.black
						border.width:		1
						height:				Theme.font.pixelSize * 2

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
							font:					Theme.font
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
						height:					plotImg.height	+ (2 * Theme.generalAnchorMargin)
						anchors
						{
							top:				title.bottom
							left:				yAxisId.right
							right:				parent.right
							margins:			Theme.generalAnchorMargin
						}

						Image
						{
							id:					plotImg
							source:				plotEditorModel.data
							sourceSize.width:	plotEditorModel.width
							sourceSize.height:	plotEditorModel.height
							width:				parent.width - ( 2 * plotImgRect.border.width)
							height:				width * (plotEditorModel.height / plotEditorModel.width) + ( 2 * plotImgRect.border.width)
							x:					plotImgRect.border.width
							y:					plotImgRect.border.width
						}
					}

					Rectangle
					{
						id:				xAxisId
						color:			Theme.white
						border.color:	Theme.black
						border.width:	preferencesModel.uiScale
						height:			Theme.font.pixelSize * 4.5

						anchors
						{
							top:		plotImgRect.bottom
							topMargin:	Theme.generalAnchorMargin
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
							implicitHeight:	Theme.font.pixelSize * 2

							Text
							{
								id:						textDelegate
								text:					display
								padding:				4 * preferencesModel.uiScale
								font:					Theme.font
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
								font:					Theme.font
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
									color:				Theme.white
									border.color:		Theme.black
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
