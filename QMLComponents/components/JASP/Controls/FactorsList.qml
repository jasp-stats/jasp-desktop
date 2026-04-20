import QtQuick
import JASP.Controls

/*!
    \qmltype FactorsList
    \inqmlmodule JASP.Controls 1.0
    \brief An assigned variables list with an editable title, used for individual factor panels.

    Extends AssignedVariablesList by adding an editable TextField for the factor title.
    Typically used inside FactorsForm; each FactorsList panel represents one factor
    and its assigned variables.

    \section1 R Binding

    \list
    \li \b{R Type:} array of variable names
    \li \b{Default:} [] (empty)
    \endlist

    \section1 Properties

    \list
    \li \b editableTitle (string) - The editable factor title text. Default: "".
    \endlist

    \section1 Signals

    \list
    \li \b titleIsChanged() - Emitted when the factor title is changed by the user.
    \endlist

    \section1 Inherited Properties from AssignedVariablesList

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b dropKeys (var) - Keys for accepting drag-and-drop. Default: [].
    \li \b allowedColumns (var) - Column types allowed. Default: [].
    \li \b allowTypeChange (bool) - Allow changing variable type icons. Default: false.
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
    FactorsList {
        name: "factor1"
        editableTitle: "Factor 1"
        dropKeys: "allAvailableVariables"
    }
    \endqml
*/
AssignedVariablesList
{
	property alias editableTitle: titleField.value
	signal titleIsChanged()
	title: " " //dummy

	TextField
    {
		id: titleField
		isBound: false
        anchors.top: parent.top
        anchors.left: parent.left
        fieldWidth: parent.width
    }

	Component.onCompleted: titleField.editingFinished.connect(titleIsChanged);

}
