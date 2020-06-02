import JASP			1.0
import JASP.Widgets	1.0
import QtQuick		2.11 as QT

TextField
{
	id							: selector
	label						: save ? qsTr("Save file to:")			: qsTr("Load file from:")
	property string	caption		: save ? qsTr("Select file to save")	: qsTr("Select file to load")
	property bool	save		: true
	property string	filter		: "*"
	property alias	buttonText	: button.text

	implicitWidth				: button.x + button.width

	RectangularButton
	{
		id				: button
		text			: qsTr("Browse")
		anchors
		{
			leftMargin	: jaspTheme.generalAnchorMargin
			left		: control.right
			top			: selector.top
			bottom		: selector.bottom
		}

		onClicked:
		{
			var browsedFile = selector.save ?
						messages.browseSaveFileDocumentsQML(selector.caption, selector.filter) :
						messages.browseOpenFileDocumentsQML(selector.caption, selector.filter) ;

			selector.value = browsedFile;
			selector.doEditingFinished();
		}

	}

}
