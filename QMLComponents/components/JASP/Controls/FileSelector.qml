import JASP
import JASP.Controls
import QtQuick		 as QT

/*!
    \qmltype FileSelector
    \inqmlmodule JASP.Controls 1.0
    \brief A text field with a browse button for selecting files or directories.

    Extends TextField with a "Browse" button that opens a native file dialog.
    Supports saving, loading, directory selection, and file type filtering.

    \section1 R Binding

    \list
    \li \b{R Type:} \c character (file path)
    \li \b{Default:} ""
    \endlist

    \section1 Properties

    \list
    \li \b save (bool) - If true, opens a save dialog; otherwise a load dialog. Default: true.
    \li \b caption (string) - Caption for the file dialog. Default: auto-generated from save.
    \li \b filter (string) - File filter pattern (e.g. "*.csv"). Default: "*".
    \li \b buttonText (string) - Text on the browse button. Default: "Browse".
    \li \b directory (bool) - If true, browses for a directory instead of a file. Default: false.
    \li \b multiple (bool) - Allow selecting multiple files (load mode only). Default: false.
    \endlist

    \section1 Inherited Properties from TextField

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b value (string) - Current file path. Default: "".
    \li \b label (string) - Label displayed before the field. Default: auto-generated from save.
    \endlist

    \section1 Other Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    FileSelector {
        name: "outputFile"
        save: true
        filter: "*.csv"
    }
    \endqml
*/
TextField
{
	id							: selector
	label						: save ? qsTr("Save file to:")			: qsTr("Load file from:")
	property string	caption		: save ? qsTr("Select file to save")	: qsTr("Select file to load")
	property bool	save		: true
	property string	filter		: "*"
	property alias	buttonText	: button.text
	property bool	directory	: false
	property bool	multiple	: false

	implicitWidth				: button.x + button.width

	RoundedButton
	{
		id				: button
		text			: qsTr("Browse")
		z				: 5
		anchors
		{
			leftMargin	: jaspTheme.generalAnchorMargin
			left		: control.right
			top			: selector.top
			bottom		: selector.bottom
		}

		onClicked:
		{
			var browsedFile;

			if (selector.directory)
				browsedFile = messages.browseOpenFolderQML(selector.caption, selector.filter)
			else if (selector.save)
				browsedFile = messages.browseSaveFileDocumentsQML(selector.caption, selector.filter)
			else
				browsedFile = messages.browseOpenFileDocumentsQML(selector.caption, selector.filter, multiple)

			selector.value = browsedFile;
			selector.doEditingFinished();
		}

	}

}
