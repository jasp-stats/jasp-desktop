import QtQuick
import JASP.Controls

Window
{
    id:	encryptWindow

    minimumWidth: Math.max(passwordInput.minWidth, jaspSubmission.implicitWidth, showAdvancedCheckbox.implicitWidth) + winPadding * 2
    minimumHeight: contentColumn.implicitHeight


    // default property alias	content:			contentInfo.children
    //         property string labelcolor:			"#F99800"
    //         property string closebuttoncolor:	jaspTheme.blue

    visible:                encryptionModel.visible
    title:					qsTr("Enter Encryption Settings")
    modality:               Qt.ApplicationModal
    color:                  jaspTheme.white

    property real winPadding: 20 * jaspTheme.uiScale

    signal closeModel();

    onCloseModel:
    {
        encryptionModel.submit();
        encryptWindow.close()
    }

    onClosing: 
    {
        if(encryptionModel.visible)
            encryptionModel.cancel();
    }

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
        id:             contentColumn
        spacing:        jaspTheme.groupContentPadding
        padding:        winPadding

        TextField {
            id: passwordInput
            text: qsTr("Password:")
            fieldWidth: encryptWindow.width - controlLabel.implicitWidth - jaspTheme.labelSpacing - 2 * winPadding

            property int minWidth: controlLabel.implicitWidth + 300 * jaspTheme.uiScale

            control.echoMode: TextInput.Password
            control.Keys.onReturnPressed: (event)=> { submitButton.onClicked() }
        }

        CheckBox {
            id: jaspSubmission
            text: qsTr("JASP Team Submission.")
            visible: !encryptionModel.readingMode
        }

        CheckBox {
            id: showAdvancedCheckbox
            text: qsTr("Advanced Settings")
        }

        Group {
            id: advancedSettings
            visible: showAdvancedCheckbox.checked

            property real fieldWidth: encryptWindow.width - Math.max(privateKey.controlLabel.implicitWidth, publicKey.controlLabel.implicitWidth) - jaspTheme.labelSpacing - 2 * winPadding

            TextField {
                id: privateKey
                label: qsTr("Private key (base64)")
                placeholderText: ""
                control.echoMode: TextInput.Password

                fieldWidth: advancedSettings.fieldWidth
            }

            TextField {
                id: publicKey
                label: qsTr("Receiver Public key (base64)")
                placeholderText: ""

                fieldWidth: advancedSettings.fieldWidth
            }
        }

        Row
        {
            id: buttons
            spacing: 10 * jaspTheme.uiScale
            property real buttonWidth: (encryptWindow.width - winPadding * 2 - buttons.spacing) / 2
            Button {
                id: submitButton
                text: qsTr("Submit")
                width: buttons.buttonWidth
                control.color: jaspTheme.blue
                onClicked: {
                    encryptionModel.encryptionActive = true;
                    encryptionModel.password = passwordInput.displayValue;
                    encryptionModel.jaspSubmission = jaspSubmission.checked;
                    encryptionModel.publickey = publicKey.displayValue;
                    encryptionModel.privatekey = privateKey.displayValue
                    closeModel();
                }
            }

            Button {
                id: cancelButton
                text: qsTr("Cancel")
                width: buttons.buttonWidth
                onClicked: {
                    encryptionModel.cancel()
                }
            }

        }

    }
}
