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

std::string JaspEncryptionData::getPublicKeyResponse()
{
	if(_jaspTeamSubmission) {
		return JASPTeamPublicKey;
	}
    return _publickeyResponse;
}

std::string JaspEncryptionData::getPasswordSaltResponse()
{
    return _passwordSaltResponse;
}

void JaspEncryptionData::reset()
{
	_parametersSet		= false;
	_encryptionActive	= false;
	_jaspTeamSubmission = false;
	
	_password.clear();
    _privatekey.clear();
	_publickeyResponse.clear();
    _passwordSaltResponse.clear();
}
