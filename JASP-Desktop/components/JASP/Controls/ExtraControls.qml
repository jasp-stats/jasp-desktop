import QtQuick 2.0
import QtQuick.Layouts 1.3

RowLayout
{
	id: extraColumns
	anchors.verticalCenter:	parent.verticalCenter
	anchors.right:			parent.right
	anchors.rightMargin:	16 * preferencesModel.uiScale
	spacing:				1
	z:						10
	layoutDirection:		Qt.RightToLeft

	property var	model
	property var	controlComponents: []


	Repeater
	{
		id:		repeater
		model: extraColumns.model

		delegate: Loader
		{
			sourceComponent: controlComponents[index]
			asynchronous:	false

			onLoaded:
			{
				item.name = model.name
				var keys = Object.keys(model.properties)
				var values = Object.values(model.properties)
				for (var i = 0; i < keys.length; i++) {
					var name = keys[i]
					if (item.hasOwnProperty(name))
						item[name] = values[i]
					else if (name === "rightMargin")
						Layout.rightMargin = values[i]
				}
				extraColumns.model.controlLoaded(item.name, item)
			}

			onStatusChanged: if (status === Loader.Error) form.addError(qsTr("Error when trying to load control: %1").arg(sourceComponent.errorString()))
		}

	}
}
