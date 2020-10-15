import QtQuick 2.12

Item
{
	id:				container
	implicitWidth:	200
	implicitHeight: 200

	Image
	{
		id						: img
		source					: jaspTheme.iconPath + "/loading.svg"
		sourceSize.width		: width
		sourceSize.height		: width
		height					: width
		width					: Math.min(parent.width, parent.height)
		transformOrigin			: Item.Center
		anchors.centerIn		: parent

		property int	segments: 12
		property real	duration: 1200
		property int	run		: 0

		onVisibleChanged: if(visible && !anim.running) anim.start()

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
