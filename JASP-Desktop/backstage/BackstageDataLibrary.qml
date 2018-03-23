import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

Item 
{
	
	Rectangle
	{
		id:rect
		anchors.fill: parent
		anchors.right: parent.right
		objectName: "rect"
		color: "#ececec"
		
		Label 
		{
			id:headlabel
			width:400
			height:30
			anchors.top: parent.top
			text: "  Data Library"
			//font.family: "SansSerif"
			font.pixelSize: 18			
		}
		
		ToolSeparator
		{
			id: firstseparator
			anchors.top: headlabel.bottom
			width: rect.width
			orientation: Qt.Horizontal
		}
		
		
		DataLibraryBreadCrumbs 
		{
			id:datalibrarybreadcrumbs
			width: rect.width
			height:60
			anchors.top: firstseparator.bottom
			anchors.left: parent.left
			anchors.right: parent.right
			
		}
		
		ToolSeparator
		{
			id: secondseparator
			anchors.top: datalibrarybreadcrumbs.bottom
			width: rect.width
			orientation: Qt.Horizontal
		}
		
		
		
		DataLibraryList
		{
			id: datalibrarylist
			width: rect.width	
			anchors.top: secondseparator.bottom
			anchors.left: parent.left
			anchors.right: parent.right
		}
		
		
//		GroupBox 
//		{
//			id: stackbox
//			anchors.top: datalibrarylist.bottom
//			title: "Pathes"
//			width: rect.width
			
//			Layout.fillWidth: true
//			Layout.fillHeight: true
			
//			StackLayout 
//			{
//				id: stackLayout
//				anchors.fill: parent
				
//				function advance(id) { 
					
//					currentIndex = (currentIndex + 1) % count
//					console.log("Count is : " + count);
//					console.log("currentIndex is : " + currentIndex);
//					console.log("Index is : " + id);
//				}
				
//				Repeater 
//				{
//					id: stackRepeater
//					model: 5
//					Rectangle 
//					{
//						id:rectrepeater
//						implicitHeight: 70
//						color: Qt.hsla((0.5 + index)/stackRepeater.count, 0.3, 0.7, 1)
//						Button 
//						{ 
//							anchors.verticalCenter: parent.verticalCenter					
//							x: {index*150;}
//							text: "Example Path " + (index + 1);																	onClicked: { stackLayout.advance(index); }
//						}
//					}
//				}
//			}
//		}		
	}
}

