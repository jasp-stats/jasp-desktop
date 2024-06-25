import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models

Image
{
	id:			colIsComputed

	width:		columnIsComputed ? headerRoot.__iconDim : 0
	height:		headerRoot.__iconDim
	visible:	columnIsComputed

	source:		columnIsInvalidated ? "" :	jaspTheme.iconPath + (columnError.length > 0 ? "/error.png" : "/computed.png")
	sourceSize {	width:	headerRoot.__iconDim * 2
					height:	headerRoot.__iconDim * 2 }
	
	
	LoadingIndicator
	{
		id:				colIsInvalidated
		anchors.fill:	parent
		visible:		columnIsInvalidated
	}
}
