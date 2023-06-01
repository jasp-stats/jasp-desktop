import QtQuick 2.0

Rectangle
{
	color: jaspTheme.white

	z: 2

	ListView
	{
		id:				listOfColumns
		orientation:	ListView.Horizontal
		anchors.fill:	parent
		boundsBehavior:	Flickable.StopAtBounds


		model: ListModel
		{
			ListElement { name: "dummyNominalText";	type: jaspTheme.iconPath + "/variable-nominal-text.png";	}
			ListElement { name: "dummyNominal";		type: jaspTheme.iconPath + "/variable-nominal.png";		}
			ListElement { name: "dummyOrdinal";		type: jaspTheme.iconPath + "/variable-ordinal.png";		}
			ListElement { name: "dummyScale";		type: jaspTheme.iconPath + "/variable-scale.png";			}
		}

		delegate: MouseArea
		{
			width: col.width
			height: col.height

			ColumnDrag {  id: col; columnName: name; columnIcon: type; alternativeDropFunction: createFunc }

			onDoubleClicked: createFunc()

			property var createFunc: function () { return columnComp.createObject(scriptColumn, { "columnName": name, "columnIcon": type, "alternativeDropFunction": null } ) }

			Component {	id: columnComp;	ColumnDrag { }	}
		}
	}
}
