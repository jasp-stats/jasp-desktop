import QtQuick 2.0


ListView {
    id                         : listOfStuff
    property string __debugName: "ElementView"
    property real   maxWidth   : 200
    property real   widthMargin: 10
    spacing                    : 4

    delegate: MouseArea {

        width : orientation === ListView.Horizontal ? elementLoader.width  : ListView.view.width
        height: orientation === ListView.Horizontal ? ListView.view.height : elementLoader.height
        z     : 5

        property var alternativeDropFunctionDef: function (caller) {
            var obj         = null
            var _columnName = undefined

            if (type == "column") {
                _columnName = columnName
                obj         = columnCompBetterContext.createObject(scriptColumn, {
                                                               toolTipText: "",
                                                               alternativeDropFunction: null,
                                                               columnName: _columnName,
                                                               acceptsDrops: true
                                                           })
            }

            return obj
        }

        Loader {
            id: elementLoader

            property bool   isColumn   : type === "column"
            property real   listWidth  : parent.width
            property string listColName: isColumn ? columnName : "???"
            property string listToolTip: ""

            //anchors.centerIn: parent
            x: isColumn ? listOfStuff.widthMargin / 2 : (parent.width - width)
                          - (listOfStuff.widthMargin / 2)

            sourceComponent: type === "column" ? columnComp : defaultComp

            onLoaded: {
                if (listOfStuff.orientation !== ListView.Horizontal && listOfStuff.width < width + listOfStuff.widthMargin)
                    listOfStuff.width = width + listOfStuff.widthMargin
            }
        }

        onDoubleClicked: alternativeDropFunctionDef()

        Component {
            id: defaultComp
            Text {
                text : "Something wrong!"
                color: "red"
            }
        }

        Component {
            id: columnComp
            ColumnDrag {
                toolTipText            : listToolTip
                columnName             : listColName
                alternativeDropFunction: alternativeDropFunctionDef
            }
        }
    }

    Component {
        id: defaultCompBetterContext
        Text { }
    }

    Component {
        id: columnCompBetterContext
        ColumnDrag { }
    }
}
