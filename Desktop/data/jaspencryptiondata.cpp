#include "jaspencryptiondata.h"

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

std::string JaspEncryptionData::getPublicKey()
{
	if(_jaspTeamSubmission) {
		return JASPTeamPublicKey;
	}
	return _publickeyToUse;
}

void JaspEncryptionData::reset()
{
	_parametersSet = false;
	_encryptionActive = false;
	_jaspTeamSubmission = false;
	_password.clear();
	_publickeyToUse.clear();
}
