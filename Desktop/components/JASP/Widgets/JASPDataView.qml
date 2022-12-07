import QtQuick			2.9
import QtQuick.Window	2.3
import QtQuick.Controls	2.2

import JASP.Controls	1.0
import JASP				1.0

FocusScope
{
	id: __JASPDataViewRoot
				property alias view:					theView
				property alias model:					theView.model
				property alias selection:				theView.selection
				property string toolTip:				""
				property alias cursorShape:				wheelCatcher.cursorShape
				property alias mouseArea:				wheelCatcher
				property bool  doubleClickWorkaround:	true

				property alias itemDelegate:			theView.itemDelegate
				property alias rowNumberDelegate:		theView.rowNumberDelegate
				property alias columnHeaderDelegate:	theView.columnHeaderDelegate
				property alias leftTopCornerItem:		theView.leftTopCornerItem
				property alias extraColumnItem:			theView.extraColumnItem
				property alias editDelegate:			theView.editDelegate
				property alias cacheItems:				theView.cacheItems

				property alias itemHorizontalPadding:	theView.itemHorizontalPadding
				property alias itemVerticalPadding:		theView.itemVerticalPadding
	readonly	property alias rowNumberWidth:			theView.rowNumberWidth
	readonly	property alias headerHeight:			theView.headerHeight

	readonly	property alias contentX:				myFlickable.contentX
	readonly	property alias contentY:				myFlickable.contentY
	readonly	property alias contentWidth:			myFlickable.contentWidth
	readonly	property alias contentHeight:			myFlickable.contentHeight
	readonly	property alias contentX0:				myFlickable.contentX
	readonly	property alias contentY0:				myFlickable.contentY
	readonly	property real  contentX1:				contentX + myFlickable.width
	readonly	property real  contentY1:				contentY + myFlickable.height

	readonly	property alias verticalScrollWidth:		vertiScroller.width
	readonly	property alias horizontalScrollHeight:	horiScroller.height
	
				property alias horiScroller:			horiScroller
				property alias vertiScroller:			vertiScroller

	readonly	property real  flickableWidth:			myFlickable.width
	readonly	property real  flickableHeight:			myFlickable.height

				property real  contentFlickSize:		100
	
	Keys.onUpPressed:		(event) => { budgeUp();		event.accepted = true; }
	Keys.onLeftPressed:		(event) => { budgeLeft();	event.accepted = true; }
	Keys.onDownPressed: 	(event) => { budgeDown();	event.accepted = true; }
	Keys.onRightPressed:	(event) => { budgeRight();	event.accepted = true; }
	
	function budgeUp()		{ if(myFlickable.contentY0 > 0)							myFlickable.contentY = Math.max(0,												myFlickable.contentY - contentFlickSize) }
	function budgeDown()	{ if(myFlickable.contentY1 < myFlickable.contentHeight)	myFlickable.contentY = Math.min(myFlickable.contentHeight - myFlickable.height,	myFlickable.contentY + contentFlickSize) }
	function budgeLeft()	{ if(myFlickable.contentX0 > 0)							myFlickable.contentX = Math.max(0,												myFlickable.contentX - contentFlickSize) }
	function budgeRight()	{ if(myFlickable.contentX1 < myFlickable.contentWidth)	myFlickable.contentX = Math.min(myFlickable.contentWidth  - myFlickable.width,	myFlickable.contentX + contentFlickSize) }
	

	function moveItemIntoView(item)
	{
		var x0 = item.x - itemHorizontalPadding;
		var x1 = item.x + itemHorizontalPadding + item.width;
		var y0 = item.y - itemHorizontalPadding;
		var y1 = item.y + itemHorizontalPadding + item.height;

		if		( x1 > contentX1 )					myFlickable.contentX = Math.max(rowNumberWidth, x1 - myFlickable.width)	;
		else if	( x0 < contentX0 + rowNumberWidth)	myFlickable.contentX =							x0 - rowNumberWidth		;
		if		( y1 > contentY1)					myFlickable.contentY = Math.max(headerHeight,	y1 - myFlickable.height);
		else if	( y0 < contentY0 + headerHeight)	myFlickable.contentY =							y0 - headerHeight		;
	}
	
	
	Keys.onPressed: (event)=>
	{
		var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier);
		var shiftPressed	= Boolean(event.modifiers & Qt.ShiftModifier  );

		if(controlPressed)
			switch(event.key)
			{
			case Qt.Key_C:
				theView.copy(shiftPressed);
				event.accepted = true;
				break;

			case Qt.Key_X:
				theView.cut(shiftPressed);
				event.accepted = true;
				break;

			case Qt.Key_V:
				theView.paste(shiftPressed);
				event.accepted = true;
				break;

			case Qt.Key_A:
					theView.selectAll();
					event.accepted = true;
				break;
			}
	}

	Flickable
	{
		id:					myFlickable
		z:					-1
		clip:				true

		anchors.top:		parent.top
		anchors.left:		parent.left
		anchors.right:		vertiScroller.left
		anchors.bottom:		horiScroller.top
		
		contentHeight:	theView.height
		contentWidth:	theView.width

		DataSetView
		{
			z:			1
			id:			theView
			model:		null
			
			/* voor Item
			x:			-myFlickable.contentX
			y:			-myFlickable.contentY
			viewportX:	 myFlickable.contentX
			viewportY:	 myFlickable.contentY
			viewportW:	 myFlickable.width	//myFlickable.visibleArea.widthRatio  * width
			viewportH:	 myFlickable.height	//myFlickable.visibleArea.heightRatio * height
			*/
			
			viewportX:	myFlickable.contentX
			viewportY:	myFlickable.contentY
			viewportW:	myFlickable.visibleArea.widthRatio  * width
			viewportH:	myFlickable.visibleArea.heightRatio * height
			
			onSelectionBudgesUp:	__JASPDataViewRoot.budgeUp()
			onSelectionBudgesDown:	__JASPDataViewRoot.budgeDown()
			onSelectionBudgesLeft:	__JASPDataViewRoot.budgeLeft()
			onSelectionBudgesRight:	__JASPDataViewRoot.budgeRight()
		}
	}
	/*
	MouseArea
	{
		id:					wheelCatcher
		anchors.fill:		myFlickable
		acceptedButtons:	Qt.NoButton 
		cursorShape:		Qt.PointingHandCursor
		z:					1000
		
	}
	*/

	signal doubleClicked()

	JASPMouseAreaToolTipped
	{
		id:					wheelCatcher
		z:					1000
		anchors.fill:		myFlickable
		anchors.leftMargin:	theView.rowNumberWidth
		anchors.topMargin:	theView.headerHeight

		toolTipText:		__JASPDataViewRoot.doubleClickWorkaround ? qsTr("Double click to edit data") : ""

		acceptedButtons:	__JASPDataViewRoot.doubleClickWorkaround ? Qt.LeftButton : Qt.NoButton
		dragging:			myFlickable.dragging
		//hoverEnabled:		!flickableInteractive

		property real	lastTimeClicked:	-1
		property real	doubleClickTime:	400

		onPressed: (mouse)=>
		{
			//console.log("doubleclick workaround pressed")
			if(!__JASPDataViewRoot.doubleClickWorkaround)
			{
				mouse.accepted = false;
				return;
			}

			var curTime = new Date().getTime()

			if(lastTimeClicked === -1 || curTime - lastTimeClicked > doubleClickTime)
			{
			//	console.log("doubleclick workaround pressed set time")
				lastTimeClicked = curTime
				mouse.accepted = false
			}
			else
			{
			//	console.log("doubleclick workaround activated")
				lastTimeClicked = -1
				__JASPDataViewRoot.doubleClicked()
			}
		}

		onWheel: (wheel)=>
		{
			if(wheel.angleDelta.y == 120)
			{
				if(wheel.modifiers & Qt.ShiftModifier)	horiScroller.scrollUp()
				else									vertiScroller.scrollUp()
			}
			else if(wheel.angleDelta.y == -120)
			{
				if(wheel.modifiers & Qt.ShiftModifier)	horiScroller.scrollDown()
				else									vertiScroller.scrollDown()
			}
			/* Might be needed to have scrolling when flickable is !interactive. But something else seems to be stealing the wheel.
			else if(!flickableInteractive)
			{
				horiScroller.scroll( -wheel.pixelDelta.x);
				vertiScroller.scroll(-wheel.pixelDelta.y);
			}*/
			else
				wheel.accepted = false;
		}
	}


	
	JASPScrollBar
	{
		id:				vertiScroller;
		flickable:		myFlickable
		anchors.top:	parent.top
		anchors.right:	parent.right
		anchors.bottom: horiScroller.top
		bigBar:			true
	}

	JASPScrollBar
	{
		id:				horiScroller;
		flickable:		myFlickable
		vertical:		false
		anchors.left:	parent.left
		anchors.right:	vertiScroller.left
		anchors.bottom: parent.bottom
		bigBar:			true
	}
}
