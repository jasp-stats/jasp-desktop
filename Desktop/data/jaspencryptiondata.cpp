#include "jaspencryptiondata.h"
#include "utilities/messageforwarder.h"

// Initialize static members
JaspEncryptionData* JaspEncryptionData::instancePtr = nullptr;
std::mutex JaspEncryptionData::mtx;

JaspEncryptionData* JaspEncryptionData::getInstance() {
    if (instancePtr == nullptr) {
        std::lock_guard<std::mutex> lock(mtx);
        if (instancePtr == nullptr) {
            instancePtr = new JaspEncryptionData();
        }
    }
    return instancePtr;
}

bool JaspEncryptionData::queryUserForPassword(bool hideInput)
{
    if(!encryptionActive())
        _password = MessageForwarder::queryTextInput(MessageForwarder::tr("Enter A Password"), MessageForwarder::tr("Password"), "", _encryptionActive, hideInput).toStdString();
    return encryptionActive();
}
