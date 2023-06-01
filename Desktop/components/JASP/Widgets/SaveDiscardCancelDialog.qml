import QtQuick	2.12
import JASP		1.0

QtObject
{
					id:			dialogRoot
	property string	title:		qsTr("Changes were made")
	property string	text:		qsTr("There are unapplied changes; what would you like to do?")

	signal			save();
	signal			cancel();
	signal			discard();

	function open()
	{
		switch(messages.showSaveDiscardCancelQML(title, text, qsTr("Apply"), qsTr("Discard"), qsTr("Cancel")))
		{
		case MessageForwarder.Save:		dialogRoot.save();		break;
		case MessageForwarder.Discard:	dialogRoot.discard();	break;
		case MessageForwarder.Cancel:	dialogRoot.cancel();	break;
		};
	}
}
