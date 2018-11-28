<<<<<<< HEAD
import QtQuick 2.10
=======
import QtQuick 2.11
>>>>>>> qmlFormsB
import JASP.Theme 1.0

FocusScope {
    id: jaspControl
    property string controlType: "JASPControl"
    property string name: ""
    property bool hasTabFocus: true
    property bool isBound: true
    property bool debug: false
<<<<<<< HEAD
    property var backgroundRectangle: Rectangle {
=======
    property bool useDefaultBackground: false
    property var controlBackground: defaultBackground
    property int backgroundWidth
    property int backgroundHeight
    
    Rectangle {
        id: defaultBackground
        visible: useDefaultBackground
>>>>>>> qmlFormsB
        color: debug ? Theme.debugBackgroundColor : Theme.analysisBackgroundColor
        border.width: 0
        border.color: Theme.analysisBackgroundColor
    }
    
    readonly property string iconFolder: "qrc:/icons/"
    
    Component.onCompleted: {
<<<<<<< HEAD
        if (typeof(DEBUG_MODE) !== "undefined" && !DEBUG_MODE && debug)
            visible = false;
=======
        if (!DEBUG_MODE && debug)
            visible = false;
        if (typeof(control) !== "undefined")
        {
            if (useDefaultBackground)
                control.background = defaultBackground
            if (backgroundWidth)
                control.background.width = Qt.binding(function (){ return backgroundWidth; })
            if (backgroundHeight)
                control.background.height = Qt.binding(function (){ return backgroundHeight; })
        }
>>>>>>> qmlFormsB
    }
    
    states: [
        State {
            when: jaspControl.activeFocus && jaspControl.hasTabFocus
            PropertyChanges {
<<<<<<< HEAD
                target: backgroundRectangle
=======
                target: controlBackground
>>>>>>> qmlFormsB
                border.width: 3
                border.color: Theme.focusBorderColor
                radius: 3
            }
        }
    ]
    
    transitions: [
        Transition {
            ParallelAnimation {
                NumberAnimation {
<<<<<<< HEAD
                    target: backgroundRectangle
=======
                    target: controlBackground
>>>>>>> qmlFormsB
                    properties: "border.width"; 
                    duration: 800; 
                    easing.type: Easing.OutElastic; 
                    easing.amplitude: 1.5;
                }
                ColorAnimation {
<<<<<<< HEAD
                    target: backgroundRectangle                    
=======
                    target: controlBackground                    
>>>>>>> qmlFormsB
                    duration: 100
                }
            }
        }
    ]
    
    
}
