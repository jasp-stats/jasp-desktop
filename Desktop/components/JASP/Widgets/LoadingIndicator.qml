import QtQuick 2.12

Item
{
	id:				container
	implicitWidth:	200
	implicitHeight: 200

	Image
	{
		id						: img
		source					: visible ? jaspTheme.iconPath + "/loading.svg" : ""
		sourceSize.width		: width
		sourceSize.height		: width
		height					: width
		width					: Math.min(parent.width, parent.height)
		transformOrigin			: Item.Center
		anchors.centerIn		: parent

		readonly property int	segments: 12
		readonly property real	duration: 1200
				 property int	run		: 0

		onVisibleChanged: 	startStopIfVisible();
		ListView.onReused:	startStopIfVisible();
		ListView.onPooled:	anim.stop()

		function startStopIfVisible()
		{
			if ( visible && !anim.running)
			{
				run = 0;
				anim.start();
			}

			if(!visible && anim.running)
				anim.stop();
		}

		SequentialAnimation
		{
			id			: anim

			loops		: 1

			onFinished :
				if(visible)
				{
					img.run ++;
					img.run %= img.segments
					start()
				}

			PropertyAction	{ value: (360 / img.segments) * img.run;	target: img; property: "rotation" }
			PauseAnimation	{ duration: img.duration / img.segments }
		}
	}
}
