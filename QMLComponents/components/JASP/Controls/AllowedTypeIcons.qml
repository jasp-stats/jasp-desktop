import QtQuick

/*!
    \qmltype AllowedTypeIcons
    \inqmlmodule JASP.Controls 1.0
    \brief Internal component that displays variable type filter icons.

    \note This is an internal UI component. Module developers do not use this control directly.
    It renders a row of icons indicating which variable types (scale, ordinal, nominal)
    are allowed in a particular VariablesList.

    \section1 Properties

    \list
    \li \b iconModel (var) - Model of icon source paths to display.
    \li \b count (int) - Number of icons currently shown.
    \endlist
*/
Row
{
	property var iconModel
	property int count: allowedColumnsId.count

	spacing: jaspTheme.contentMargin

	Repeater
	{
		id:		allowedColumnsId
		model:	iconModel

		Image
		{
			source:		modelData
			height:		16 * preferencesModel.uiScale
			width:		16 * preferencesModel.uiScale
			z:			2
			mipmap:		true
			smooth:		true
		}
	}
}
