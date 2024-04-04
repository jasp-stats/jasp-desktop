import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models

Image
{
	id:			colIsComputed

	width:		headerRoot.__iconDim
	height:		headerRoot.__iconDim
	visible:	columnIsComputed

	source:					jaspTheme.iconPath + "/computed.png"
	sourceSize {	width:	headerRoot.__iconDim * 2
					height:	headerRoot.__iconDim * 2 }

}
