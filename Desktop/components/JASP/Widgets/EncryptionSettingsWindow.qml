import QtQuick
import QtQuick.Controls as QC
import QtQuick.Layouts
import JASP.Widgets
import JASP.Controls

Window
{
    id:	encryptWindow

    width: contentColumn.implicitWidth + jaspTheme.generalAnchorMargin
    height: contentColumn.implicitHeight + jaspTheme.generalAnchorMargin
    minimumWidth: 450


    // default property alias	content:			contentInfo.children
    //         property string labelcolor:			"#F99800"
    //         property string closebuttoncolor:	jaspTheme.blue

    visible:				encryptionModel.visible
    title:					qsTr("Enter Encryption Settings")

    color:                  jaspTheme.white

    signal closeModel();

    onCloseModel:
    {
        encryptionModel.visible = false
        encryptionModel.submit();
        encryptWindow.close()
    }

    onClosing: closeModel()

    Shortcut { onActivated: closeModel();	sequences: ["Ctrl+Q", "Ctrl+W", Qt.Key_Close]; }

    Connections
    {
        target:			mainWindow
        function onCloseWindows() { closeModel(); }
    }

    Column {
        id: contentColumn
        spacing: jaspTheme.groupContentPadding
        anchors.centerIn: parent
        anchors.margins: jaspTheme.contentMargin * 2

        TextField {
            id: passwordInput
            text: qsTr("Password:")
            // echoMode: TextInput.Password
            enabled: !disableCheckbox.checked
            width: 300 * preferencesModel.uiScale
        }

        CheckBox {
            id: jaspSubmission
            text: qsTr("JASP Team Submission.")
        }

        // CheckBox {
        //     id: showAdvancedCheckbox
        //     text: "Show advanced settings"
        // }

        // Column {
        //     id: advancedSettings
        //     visible: showAdvancedCheckbox.checked

        //     Text {
        //         text: "Advanced setting:"
        //     }

        //     TextField {
        //         id: publicKey
        //         placeholderText: "Enter Receivers Publickey"
        //         width: 300 * preferencesModel.uiScale
        //     }
        // }

        Button {
            id: submitButton
            text: qsTr("Submit")
            width: 300 * preferencesModel.uiScale
            onClicked: {
                encryptionModel.encryptionActive = true;
                encryptionModel.password = passwordInput.displayValue;
                encryptionModel.jaspSubmission = jaspSubmission.checked;
                closeModel();
            }
        }

    }
}
