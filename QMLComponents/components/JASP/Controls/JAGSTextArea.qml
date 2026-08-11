import QtQuick
import JASP.Controls
import JASP

/*!
    \qmltype JAGSTextArea
    \inqmlmodule JASP.Controls 1.0
    \brief A text area preset for writing JAGS model code.

    Extends TextArea with the JAGS model text type, line numbers, and R syntax
    highlighting. Used in analyses that require user-specified JAGS models.

    \section1 R Binding

    \list
    \li \b{R Type:} \c character (JAGS model string)
    \li \b{Default:} ""
    \endlist

    \section1 Inherited Properties from TextArea

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b text (string) - Current text content. Default: "".
    \li \b label (string) - Label displayed above the text area. Default: "".
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
    JAGSTextArea {
        name: "model"
        label: qsTr("JAGS Model")
    }
    \endqml
*/
TextArea
{
	textType: JASP.TextTypeJAGSmodel
	showLineNumber: true
	
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("A JAGS text area %1").arg(title) : info
	
	RSyntaxHighlighterQuick
	{
		textDocument:		parent.textDocument
	}
}
