import QtQuick
import QtQuick.Controls as QC
import QtQuick.Layouts
import JASP.Widgets
import JASP.Controls

Window
{
    id:	encryptWindow

    width: contentColumn.implicitWidth + 50 * jaspTheme.uiScale
    height: contentColumn.implicitHeight + 50 * jaspTheme.uiScale


    // default property alias	content:			contentInfo.children
    //         property string labelcolor:			"#F99800"
    //         property string closebuttoncolor:	jaspTheme.blue

    visible:                encryptionModel.visible
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

    onVisibleChanged: {
        passwordInput.displayValue = ""
        privateKey.displayValue = ""
        publicKey.displayValue = ""
        showAdvancedCheckbox.checked = false
        jaspSubmission.checked = false
        passwordInput.forceActiveFocus();
    }

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
            width: 300 * preferencesModel.uiScale
            control.echoMode: TextInput.Password
            control.Keys.onReturnPressed: (event)=> { submitButton.onClicked() }
        }

        CheckBox {
            id: jaspSubmission
            text: qsTr("JASP Team Submission.")
        }

        CheckBox {
            id: showAdvancedCheckbox
            text: "Advanced Settings"
        }

        Group {
            id: advancedSettings
            visible: showAdvancedCheckbox.checked

            TextField {
                id: privateKey
                label: "Private key (base64):"
                placeholderText: ""
                control.echoMode: TextInput.Password
            }

            TextField {
                id: publicKey
                label: "Receiver Public key (base64):"
                placeholderText: ""
            }
        }

        Button {
            id: submitButton
            text: qsTr("Submit")
            width: parent.width
            onClicked: {
                encryptionModel.encryptionActive = true;
                encryptionModel.password = passwordInput.displayValue;
                encryptionModel.jaspSubmission = jaspSubmission.checked;
                encryptionModel.publickey = publicKey.displayValue;
                encryptionModel.privatekey = privateKey.displayValue
                closeModel();
            }
        }

    }
}
