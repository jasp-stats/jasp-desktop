import QtQuick
import JASP.Controls

Window
{
    id:	encryptWindow

	Accessible.name:	title

    minimumWidth:           Math.max(passwordInput.minWidth, jaspSubmission.implicitWidth, showAdvancedCheckbox.implicitWidth) + windowPadding * 2 + 150 * jaspTheme.uiScale 
    minimumHeight:          contentColumn.implicitHeight + (advancedSettings.visible ? advancedSettings.implicitHeight + jaspTheme.groupContentPadding : 0) + buttons.height + windowPadding * 3
    visible:                encryptionModel.visible
    title:					qsTr("Enter Encryption Settings")
    modality:               Qt.ApplicationModal
    color:                  jaspTheme.white
    //transientParent:        mainWindowRoot

    property real windowPadding: 20 * jaspTheme.uiScale

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

    onVisibleChanged: if (visible) {
        passwordInput.displayValue = ""
        privateKey.displayValue = ""
        publicKey.displayValue = ""
        showAdvancedCheckbox.checked = false
        jaspSubmission.checked = false
        passwordInput.forceActiveFocus();
    }

    Shortcut { onActivated: encryptionModel.cancel();	sequences: ["Ctrl+Q", Qt.Key_Close]; }

    Connections
    {
        target:			mainWindow
        function onCloseWindows() { encryptionModel.cancel(); }
    }

    Column
    {
        spacing:        jaspTheme.groupContentPadding
        leftPadding:    windowPadding
        topPadding:     windowPadding

        Column
        {
            id:             contentColumn
            spacing:        jaspTheme.groupContentPadding

            TextField {
                id:         passwordInput
                text:       qsTr("Password:")
                fieldWidth: encryptWindow.width - controlLabel.implicitWidth - jaspTheme.labelSpacing - 2 * windowPadding

                property int minWidth: controlLabel.implicitWidth + 300 * jaspTheme.uiScale

                showEyeInside: true
                control.echoMode: TextInput.Password
                control.Keys.onReturnPressed: (event)=> { submitButton.onClicked() }
            }

            CheckBox {
                id:         jaspSubmission
                text:       qsTr("JASP Team Submission.")
                visible:    !encryptionModel.readingMode
                info:       qsTr("Click here if you want the JASP Team be able to open the file")
            }

            CheckBox {
                id:         showAdvancedCheckbox
                text:       qsTr("Advanced Settings")
            }
        }

        Group
        {
            id: advancedSettings
            visible: showAdvancedCheckbox.checked

            property real fieldWidth: encryptWindow.width - Math.max(privateKey.controlLabel.implicitWidth, publicKey.controlLabel.implicitWidth) - jaspTheme.labelSpacing - 2 * windowPadding

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
    }

    Row
    {
        id: buttons
        spacing: 10 * jaspTheme.uiScale

        anchors.bottom:         parent.bottom
        anchors.bottomMargin:   windowPadding
        anchors.left:           parent.left
        anchors.leftMargin:     windowPadding

        property real buttonWidth: (encryptWindow.width - windowPadding * 2 - buttons.spacing) / 2
        Button
        {
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

        Button
        {
            id: cancelButton
            text: qsTr("Cancel")
            width: buttons.buttonWidth
            onClicked: {
                encryptionModel.cancel()
            }
        }

    }
}
