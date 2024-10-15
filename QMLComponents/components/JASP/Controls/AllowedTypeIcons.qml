import QtQuick

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
