import QtQuick
import QtQuick.Layouts
import QtQuick.Controls as QtControls
import JASP.Controls
import JASP

/*!
    \qmltype FactorsFormModelView
    \inqmlmodule JASP.Controls 1.0
    \brief The full-width view of one model of a FactorsForm: its terms in full, one per row, each removable.

    Internal to FactorsForm (opened with the expand icon in the corner of a model's list). The terms live in the model's
    list (FactorsFormBase::modelTermRows, removeModelTerm); this view only shows them.
*/
Rectangle
{
	id: view

	property var	factorsForm
	property int	factorIndex:	-1

	readonly property var rows:		{ factorsForm.modelsRevision; return factorIndex >= 0 ? factorsForm.modelTermRows(factorIndex) : []; }
	readonly property string title:	{ var titles = factorsForm.factorsTitles; return factorIndex >= 0 && factorIndex < titles.length ? titles[factorIndex].label : ""; }

	signal back()

	color:			jaspTheme.controlBackgroundColor
	border.width:	1
	border.color:	jaspTheme.borderColor
	radius:			jaspTheme.borderRadius

	// The FactorsForm gives the view the height of its lists, so the page does not move: the title and Back stay, the
	// terms scroll.
	Label
	{
		id:		titleLabel
		x:		jaspTheme.contentMargin
		y:		jaspTheme.contentMargin
		text:	view.title
	}

	Flickable
	{
		id:					termsFlickable
		contentHeight:		content.implicitHeight
		clip:				true
		boundsBehavior:		Flickable.StopAtBounds
		onContentHeightChanged: returnToBounds()
		anchors
		{
			top:			titleLabel.bottom
			topMargin:		4 * jaspTheme.uiScale
			bottom:			buttons.top
			bottomMargin:	4 * jaspTheme.uiScale
			left:			parent.left
			leftMargin:		jaspTheme.contentMargin
			right:			parent.right
			rightMargin:	jaspTheme.contentMargin
		}

		Column
		{
			id:			content
			width:		termsFlickable.width - (scrollBar.visible ? scrollBar.width + 4 * jaspTheme.uiScale : 0)
			spacing:	4 * jaspTheme.uiScale

			Label
			{
				visible:	view.rows.length === 0
				text:		qsTr("No terms: assign them with Back.")
				color:		jaspTheme.textDisabled
			}

			Repeater
			{
				model: view.rows

				RowLayout
				{
					id:			rowItem
					required property var	modelData
					required property int	index

					width:		content.width
					spacing:	8 * jaspTheme.uiScale

					Label // the full term: the point of this view
					{
						text:					rowItem.modelData.label
						wrapMode:				Text.WrapAtWordBoundaryOrAnywhere
						Layout.fillWidth:		true
					}

					Image
					{
						source:					jaspTheme.iconPath + "cross.png"
						Layout.preferredWidth:	jaspTheme.iconSize
						Layout.preferredHeight:	jaspTheme.iconSize

						QtControls.ToolTip.text:				qsTr("Remove this term from the model")
						QtControls.ToolTip.visible:				removeMouseArea.containsMouse
						QtControls.ToolTip.toolTip.background:	Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }

						MouseArea
						{
							id:				removeMouseArea
							anchors.fill:	parent
							hoverEnabled:	true
							cursorShape:	Qt.PointingHandCursor
							onClicked:		{ var form = view.factorsForm, model = view.factorIndex, term = rowItem.index; Qt.callLater(function() { form.removeModelTerm(model, term); }); }
						}
					}
				}
			}
		}
	}

	JASPScrollBar
	{
		id:				scrollBar
		flickable:		termsFlickable
		manualAnchor:	true
		vertical:		true
		anchors
		{
			top:		termsFlickable.top
			bottom:		termsFlickable.bottom
			right:		termsFlickable.right
		}
	}

	Item
	{
		id:		buttons
		height:	backButton.height
		anchors
		{
			bottom:			parent.bottom
			bottomMargin:	jaspTheme.contentMargin
			left:			parent.left
			leftMargin:		jaspTheme.contentMargin
			right:			parent.right
			rightMargin:	jaspTheme.contentMargin
		}

		Button
		{
			id:				backButton
			anchors.right:	parent.right
			text:			qsTr("Back")
			toolTip:		qsTr("Back to the models")
			onClicked:		view.back()
		}
	}
}
