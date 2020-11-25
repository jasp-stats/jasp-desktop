import QtQuick				2.14
import QtQuick.Controls		2.14
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
				id:			focusCatcher	//Because then people can click away from the axistableview or something
				onClicked:	forceActiveFocus()
			}

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
				id:					axes
				width:				500 * jaspTheme.uiScale
				color:				jaspTheme.uiBackground
				border.color:		jaspTheme.uiBorder
				border.width:		1

				anchors
				{
					top:			parent.top
					left:			parent.left
					bottom:			parent.bottom
				}


				PlotEditingAxis
				{
					id:				xAxis
					title:			qsTr("X axis")
					axisModel:		plotEditorModel.xAxis

					anchors
					{
						top:		parent.top
						left:		parent.left
						right:		parent.right
						margins:	jaspTheme.generalAnchorMargin

					}
				}

				Rectangle
				{
					id:				axisSeparator
					height:			1
					color:			jaspTheme.uiBorder
					anchors
					{
						top:		xAxis.bottom
						topMargin:	jaspTheme.generalAnchorMargin
						left:		parent.left
						right:		parent.right
					}
				}

				PlotEditingAxis
				{
					id:				yAxis
					title:			qsTr("Y axis")
					axisModel:		plotEditorModel.yAxis

					anchors
					{
						top:		axisSeparator.bottom
						left:		parent.left
						right:		parent.right
						margins:	jaspTheme.generalAnchorMargin
					}
				}
			}

			Item
			{
				id:					plotImgParent

				anchors
				{
					top:			title.bottom
					left:			axes.right
					right:			parent.right
					bottom:			parent.bottom
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
